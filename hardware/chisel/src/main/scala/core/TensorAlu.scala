/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

// Modified by contributors from Intel Labs

package vta.core

import chisel3._
import chisel3.util._
import vta.util.config._
import vta.verif.{TraceMgr => trace_mgr}

/** ALU datapath */
class Alu(implicit p: Parameters) extends Module {
  val aluBits = p(CoreKey).accBits
  val io = IO(new Bundle {
    val opcode = Input(UInt(C_ALU_OP_BITS.W))
    val a = Input(SInt(aluBits.W))
    val b = Input(SInt(aluBits.W))
    val y = Output(SInt(aluBits.W))
  })

  // FIXME: the following three will change once we support properly SHR and SHL
  val ub = io.b.asUInt
  val width = log2Ceil(aluBits)
  val m = ~ub(width - 1, 0) + 1.U
  val n = ub(width - 1, 0)
  val a = Wire(SInt(8.W))
  val b = Wire(SInt(8.W))
  a := io.a
  b := io.b

  val negb = (~ub + 1.U).asSInt
  val fop = Seq(Mux(io.a < io.b, io.a, io.b),
    Mux(io.a < io.b, io.b, io.a),
    io.a + io.b,
    io.a >> n,
    Mux(io.a < negb, negb, Mux(io.a > io.b, io.b, io.a)),
    io.b,
    a * b,
    io.a << m
  )

  val opmux = Seq.tabulate(ALU_OP_NUM)(i => ALU_OP(i) -> fop(i))
  io.y := MuxLookup(io.opcode, io.a, opmux)
}

/** Pipelined ALU */
class AluReg(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(C_ALU_OP_BITS.W))
    val a = Flipped(ValidIO(UInt(p(CoreKey).accBits.W)))
    val b = Flipped(ValidIO(UInt(p(CoreKey).accBits.W)))
    val y = ValidIO(UInt(p(CoreKey).accBits.W))
  })
  val alu = Module(new Alu)
  val rA = RegEnable(io.a.bits, io.a.valid)
  val rB = RegEnable(io.b.bits, io.b.valid)
  val valid = RegNext(io.b.valid)

  alu.io.opcode := io.opcode

  // register input
  alu.io.a := rA.asSInt
  alu.io.b := rB.asSInt

  // output
  io.y.valid := valid
  io.y.bits := alu.io.y.asUInt
}

/** Vector of pipeline ALUs */
class AluVector(implicit p: Parameters) extends Module {
  val aluBits = p(CoreKey).accBits
  val io = IO(new Bundle {
    val opcode = Input(UInt(C_ALU_OP_BITS.W))
    val acc_a = new TensorMasterData(tensorType = "acc")
    val acc_b = new TensorMasterData(tensorType = "acc")
    val acc_y = new TensorClientData(tensorType = "acc")
    val out = new TensorClientData(tensorType = "out")
  })
  val blockOut = p(CoreKey).blockOut / p(CoreKey).blockOutFactor
  val batch = p(CoreKey).batch

  val f = Seq.fill(batch)(Seq.fill(blockOut)(Module(new AluReg)))
  val valid = Wire(Vec(batch, Vec(blockOut, Bool())))

  io.out.data.bits := DontCare // out is not fully initialized by a single module
  for (b <- 0 until batch) {
    for (i <- 0 until blockOut) {
      f(b)(i).io.opcode := io.opcode
      f(b)(i).io.a.valid := io.acc_a.data.valid
      f(b)(i).io.a.bits := io.acc_a.data.bits(b)(i)
      f(b)(i).io.b.valid := io.acc_b.data.valid
      f(b)(i).io.b.bits := io.acc_b.data.bits(b)(i)
      valid(b)(i) := f(b)(i).io.y.valid
      io.acc_y.data.bits(b)(i) := f(b)(i).io.y.bits
      io.out.data.bits(b)(i) := f(b)(i).io.y.bits
    }
  }
  io.acc_y.data.valid := valid.asUInt.andR
  io.out.data.valid := valid.asUInt.andR
}

class TensorAluIndexGenerator(debug: Boolean = false)(implicit p: Parameters) extends Module {
  val cnt_o_width = (new AluDecode).lp_0.getWidth
  val cnt_i_width = (new AluDecode).lp_1.getWidth

  val io = IO(new Bundle {
    val start = Input(Bool())
    val last = Output(Bool())
    val dec = Input(new AluDecode)
    val valid = Output(Bool())
    val src_valid = Output(Bool())
    val dst_idx = Output(UInt(new TensorParams(tensorType="acc").memAddrBits.W))
    val src_idx = Output(UInt(new TensorParams(tensorType="acc").memAddrBits.W))
    val uop_idx = Output(UInt(log2Ceil(p(CoreKey).uopMemDepth).W))
    val cnt_o = Output(UInt(cnt_o_width.W))
    val cnt_i = Output(UInt(cnt_i_width.W))
  })

  io.last := false.B

  val running = RegInit( false.B)
  val stutter = RegInit( false.B)

  val advance = io.dec.alu_use_imm || stutter

  when( !running && io.start) {
    running := true.B
  } .elsewhen( running && !advance) {
    stutter := true.B
  } .elsewhen( running && advance) {
    when ( io.last) {
      running := false.B
    }
    stutter := false.B
  }

  val cnt_i = Reg( chiselTypeOf(io.dec.lp_1))
  val dst_i = Reg( chiselTypeOf(io.dst_idx))
  val src_i = Reg( chiselTypeOf(io.src_idx))

  val cnt_o = Reg( chiselTypeOf(io.dec.lp_0))
  val dst_o = Reg( chiselTypeOf(io.dst_idx))
  val src_o = Reg( chiselTypeOf(io.src_idx))

  val uop_idx = Reg( chiselTypeOf(io.dec.uop_end))

  io.valid := running && advance
  io.src_valid := running && !advance
  io.dst_idx := dst_i
  io.src_idx := src_i
  io.uop_idx := uop_idx
  io.cnt_o := cnt_o
  io.cnt_i := cnt_i

  when( !running) {
    cnt_i := 0.U; dst_i := 0.U; src_i := 0.U;
    cnt_o := 0.U; dst_o := 0.U; src_o := 0.U;
    uop_idx := io.dec.uop_begin
  } .elsewhen (advance) {
    when (uop_idx =/= io.dec.uop_end - 1.U) {
      uop_idx := uop_idx + 1.U
    }.otherwise {
      uop_idx := io.dec.uop_begin
      when ( cnt_i =/= io.dec.lp_1 - 1.U) {
        cnt_i := cnt_i + 1.U
        dst_i := dst_i + io.dec.dst_1
        src_i := src_i + io.dec.src_1
      }.otherwise {
        when ( cnt_o =/= io.dec.lp_0 - 1.U) {
          val dst_tmp = dst_o + io.dec.dst_0
          val src_tmp = src_o + io.dec.src_0
          cnt_o := cnt_o + 1.U
          dst_o := dst_tmp
          src_o := src_tmp
          cnt_i := 0.U
          dst_i := dst_tmp
          src_i := src_tmp
        } .otherwise {
          io.last := true.B
        }
      }
    }
  }
}

class TensorAluIfc(implicit p: Parameters) extends Module {
  val aluBits = p(CoreKey).accBits
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val dec = Input(new AluDecode)
    val uop = new UopMaster
    val acc = new TensorMaster(tensorType = "acc")
    val out = new TensorMaster(tensorType = "out")
  })
}

class TensorAluPipelined(debug: Boolean = false)(implicit p: Parameters) extends TensorAluIfc {
  val stateBits = 2
  val inflightBits = 4
  val dataSplitFactor = p(CoreKey).blockOutFactor

  val sIdle::sRun::sWait::Nil = Enum(3)
  val state = RegInit(init=sIdle)
  val inflight = RegInit(0.U(inflightBits.W))

  val index_generator = Module(new TensorAluIndexGenerator)
  val aluDataReadPipeDelay = JSONCoreOptions.pipeDelayAluRdData()

  // State Machine for compute io.done correctly
  io.done := false.B
  when( state === sIdle && io.start) {
    state := sRun
  }.elsewhen( state === sRun && index_generator.io.last) {
    state := sWait
  }.elsewhen( state === sWait && inflight === 0.U) {
    state := sIdle
    io.done := true.B
  }

  index_generator.io.start := io.start
  index_generator.io.dec := io.dec

  // second term works around funny clearing in uop register file flopped output
  io.uop.idx.valid := index_generator.io.valid || index_generator.io.src_valid
  io.uop.idx.bits := index_generator.io.uop_idx

  val valid_001 = ShiftRegister( index_generator.io.valid, aluDataReadPipeDelay + 1, resetData=false.B, en = true.B)
  val valid_002 = RegNext( valid_001, init=false.B)
  val valid_003 = RegNext( valid_002, init=false.B)
  val valid_004 = RegNext( valid_003, init=false.B)

  when( index_generator.io.valid && valid_004) {
  }.elsewhen( index_generator.io.valid) {
    assert( inflight =/= ((1<<inflightBits)-1).U)
    inflight := inflight + 1.U
  }.elsewhen( valid_004) {
    assert( inflight =/= 0.U)
    inflight := inflight - 1.U
  }
  when( state === sIdle) {
    assert( inflight === 0.U)
    inflight := 0.U
  }

  val src_valid_001 = ShiftRegister(
    index_generator.io.src_valid,
    aluDataReadPipeDelay + 1,
    resetData=false.B, en = true.B)
  val src_valid_002 = RegNext( src_valid_001, init=false.B)
  val src_valid_003 = RegNext( src_valid_002, init=false.B)
  val src_valid_004 = RegNext( src_valid_003, init=false.B)

  val dst_idx_001 = ShiftRegister( index_generator.io.dst_idx, aluDataReadPipeDelay + 1)
  val src_idx_001 = ShiftRegister( index_generator.io.src_idx, aluDataReadPipeDelay + 1)

  val uop_data_001 = ShiftRegister(io.uop.data, aluDataReadPipeDelay)

  val dst_offset = uop_data_001.bits.u0

  val w = dst_offset.getWidth
  val u2 = uop_data_001.bits.u2.asTypeOf(UInt(w.W))
  val s = log2Ceil(p(CoreKey).inpMemDepth)
  val u1 = uop_data_001.bits.u1.asTypeOf(UInt(w.W))
  val src_offset = (u2 << s) | u1

  // split registers of stage 2 by data groups
  //val accRdIdxValid = valid_002 || src_valid_002
  val accRdIdxValid = valid_001 || src_valid_001
  for (idx <- 0 until dataSplitFactor) {
    //io.acc.rd(idx).idx.valid := accRdIdxValid
    io.acc.rd(idx).idx.valid := RegNext(accRdIdxValid)
  }

  val new_src_idx_001 = src_idx_001 + src_offset
  val src_idx_002 = RegNext( new_src_idx_001)
  val src_idx_003 = RegNext( src_idx_002)

  val new_dst_idx_001 = dst_idx_001 + dst_offset
  val dst_idx_002 = RegNext( new_dst_idx_001)
  val dst_idx_003 = RegNext( dst_idx_002)
  val dst_idx_004 = RegNext( dst_idx_003)

  // split registers of stage 2 by data groups
  val accRdIdxBits = Mux( src_valid_001 || io.dec.alu_use_imm, new_src_idx_001, new_dst_idx_001)
  for (idx <- 0 until dataSplitFactor) {
    io.acc.rd(idx).idx.bits := RegNext(accRdIdxBits)
    assert( io.acc.rd(idx).data.valid === (valid_003 || src_valid_003))
  }

  require(io.out.splitWidth == 1 && io.out.splitLength == 1, "-F- Out split write is not supported")
  val numVecUnits = dataSplitFactor
  val outData = Wire(io.out.wr(0).bits.data.cloneType)
  val dataRemapB = Wire(Vec(numVecUnits, io.acc.rd(0).data.bits.cloneType))
  val dataRemapA = Wire(Vec(numVecUnits, io.acc.rd(0).data.bits.cloneType))
  // numVecUnits is a pow of 2
  // split dec bits pipe further if there are many vecUnits
  val decSplitNb0 =  if (numVecUnits < 8) 1 else 2
  val decSplit0 = Wire(Vec(decSplitNb0, io.dec.cloneType))
  for (idx <- 0 until decSplitNb0) {
    decSplit0(idx) := ShiftRegister(io.dec, if(aluDataReadPipeDelay < 2) 0 else 1)
  }

  for (idx <- 0 until numVecUnits) {
    val alu = Module(new AluVector)

    for(aluLenIdx <- 0 until alu.io.acc_b.lenSplit) {
      for(aluWdtIdx <- 0 until alu.io.acc_b.widthSplit) {
        val (accGrpIdx, accLenIdx, accWdtIdx) =
          alu.io.acc_b.reindexDataFromGroup(idx, aluLenIdx, aluWdtIdx)
        dataRemapB(idx)(aluLenIdx)(aluWdtIdx) :=
          io.acc.rd(accGrpIdx).data.bits(accLenIdx)(accWdtIdx)
      }
    }
    val save_src = RegNext(dataRemapB(idx))
    val tensorImm = Wire(new TensorClientData(tensorType = "acc"))
    tensorImm.data.valid := RegNext(valid_002) //valid_003 split
    val tensorImmBits_piped = ShiftRegister(
      decSplit0(idx/(numVecUnits/decSplitNb0)).alu_imm,
      if(aluDataReadPipeDelay < 2) aluDataReadPipeDelay else aluDataReadPipeDelay -1)
    tensorImm.data.bits.foreach { b =>
      b.foreach { c =>
        c := Mux(tensorImmBits_piped(C_ALU_IMM_BITS - 1),
          Cat(-1.S((aluBits - C_ALU_IMM_BITS).W), tensorImmBits_piped), tensorImmBits_piped)
      }
    }

    // alu
    val tensorOpBits_piped = ShiftRegister(
    decSplit0(idx/(numVecUnits/decSplitNb0)).alu_op,
    if(aluDataReadPipeDelay < 2) aluDataReadPipeDelay else aluDataReadPipeDelay -1)
    val isSHR = (tensorOpBits_piped === ALU_OP(3))
    val neg_shift = isSHR & tensorImmBits_piped(C_ALU_IMM_BITS - 1)
    val fixme_alu_op = Mux(
      neg_shift,
      ALU_OP((1 << C_ALU_DEC_BITS) - 1),
      tensorOpBits_piped) // all bits set is neg shift
    alu.io.opcode := fixme_alu_op

    assert( !valid_003 || io.acc.rd(idx).data.valid)

    alu.io.acc_a.data.valid := RegNext(valid_002) //valid_003 split

    for(aluLenIdx <- 0 until alu.io.acc_a.lenSplit) {
      for(aluWdtIdx <- 0 until alu.io.acc_a.widthSplit) {
        val (accGrpIdx, accLenIdx, accWdtIdx) =
          alu.io.acc_a.reindexDataFromGroup(idx, aluLenIdx, aluWdtIdx)
        dataRemapA(idx)(aluLenIdx)(aluWdtIdx) :=
          io.acc.rd(accGrpIdx).data.bits(accLenIdx)(accWdtIdx)
        alu.io.acc_a.data.bits := dataRemapA(idx)
      }
    }
    val tensorUseImmBits_piped = ShiftRegister(
    decSplit0(idx/(numVecUnits/decSplitNb0)).alu_use_imm,
    if(aluDataReadPipeDelay < 2) aluDataReadPipeDelay else aluDataReadPipeDelay -1)
    alu.io.acc_b.data.valid := Mux(tensorUseImmBits_piped,
      tensorImm.data.valid,
      valid_003)
    alu.io.acc_b.data.bits := Mux(tensorUseImmBits_piped,
      tensorImm.data.bits,
      save_src)

    assert( alu.io.acc_y.data.valid === valid_004)
    io.acc.wr(idx).valid := RegNext(valid_003) //valid_004 split
    io.acc.wr(idx).bits.idx := RegNext(dst_idx_003)//dst_idx_004 split

    for(aluLenIdx <- 0 until alu.io.acc_y.lenSplit) {
      for(aluWdtIdx <- 0 until alu.io.acc_y.widthSplit) {
        val (accGrpIdx, accLenIdx, accWdtIdx) =
          alu.io.acc_y.reindexDataFromGroup(idx, aluLenIdx, aluWdtIdx)
        io.acc.wr(accGrpIdx).bits.data(accLenIdx)(accWdtIdx) :=
          alu.io.acc_y.data.bits(aluLenIdx)(aluWdtIdx)
      }
    }

    assert( alu.io.out.data.valid === valid_004)
    for (idx1 <- 0 until io.out.tensorLength) {
      for (idx2 <- 0 until io.out.tensorWidth/numVecUnits) {
        outData(idx1)(idx*io.out.tensorWidth/numVecUnits + idx2) := alu.io.out.data.bits(idx1)(idx2)
      }
    }
  }

// comment for split write
  io.out.wr(0).valid := valid_004
  io.out.wr(0).bits.idx := dst_idx_004
  io.out.wr(0).bits.data := outData
  io.out.tieoffRead()

  val bypass_dst = valid_003 && valid_004 && ( dst_idx_004 === dst_idx_003)
  val bypass_src = src_valid_003 && valid_004 && ( dst_idx_004 === src_idx_003)

  // Do we need a bypass
  when ( bypass_dst) {
    printf( "Bypass required on dst_idx read %x RAW with write %x\n", dst_idx_003, dst_idx_004)
    assert( false.B, "DST bypass required")
  }
  when ( bypass_src) {
    printf( "Bypass required on src_idx read %x RAW with write %x\n", src_idx_003, dst_idx_004)
    assert( false.B, "SRC bypass required")
  }

  // trace
  if (p(VerifKey).trace) {
    val write_back_stage = 4
    val cnt_o = ShiftRegister( index_generator.io.cnt_o, write_back_stage)
    val cnt_i = ShiftRegister( index_generator.io.cnt_i, write_back_stage)
    val uop_idx = ShiftRegister( index_generator.io.uop_idx, write_back_stage)
    when(io.acc.wr(io.acc.closestIOGrpIdx).valid) {
      trace_mgr.Event("ALU_ITR", "%x %x %x %x",
      cnt_o, cnt_i, uop_idx, io.acc.wr(0).bits.idx)
      io.acc.wr(0).bits.data.foreach { tensor =>
        tensor.foreach { elem =>
          //for (i <- 0 until elem.getWidth by 8) {
          //  trace_mgr.Event("+ALU_ITR", " %x", elem(i+7,i))
          //}
          // Only the first byte for now.
          //trace_mgr.Event("+ALU_ITR", " %x", elem(7, 0))
          trace_mgr.Event("+ALU_ITR", " %x", elem)
        }
      }
      trace_mgr.Event("+ALU_ITR", "\n")
    }

    val dec = io.dec
    val inst = dec.asUInt
    when(io.start) {
      when(dec.alu_use_imm) {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("EXE", "MINI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("EXE", "MAXI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("EXE", "ADDI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("EXE", "SHRI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("EXE", "CLPI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("EXE", "MOVI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("EXE", "MULI %x\n", inst)
        }
      }.otherwise {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("EXE", "MIN  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("EXE", "MAX  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("EXE", "ADD  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("EXE", "SHR  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("EXE", "CLP  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("EXE", "MOV  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("EXE", "MUL  %x\n", inst)
        }
      }
      trace_mgr.Event("ALU_LOOP", "%x %x %x %x\n",
      dec.lp_0, dec.lp_1, dec.uop_begin, dec.uop_end)
    }.elsewhen(io.done) {
      when(dec.alu_use_imm) {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("RET", "MINI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("RET", "MAXI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("RET", "ADDI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("RET", "SHRI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("RET", "CLPI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("RET", "MOVI %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("RET", "MULI %x\n", inst)
        }
      }.otherwise {
        when(dec.alu_op === ALU_OP(0)) {
          trace_mgr.Event("RET", "MIN  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(1)) {
          trace_mgr.Event("RET", "MAX  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(2)) {
          trace_mgr.Event("RET", "ADD  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(3)) {
          trace_mgr.Event("RET", "SHR  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(4)) {
          trace_mgr.Event("RET", "CLP  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(5)) {
          trace_mgr.Event("RET", "MOV  %x\n", inst)
        }.elsewhen(dec.alu_op === ALU_OP(6)) {
          trace_mgr.Event("RET", "MUL  %x\n", inst)
        }
      }
    }
  }
}
class TensorAlu(debug: Boolean = false)(implicit p: Parameters) extends TensorAluPipelined(debug)
