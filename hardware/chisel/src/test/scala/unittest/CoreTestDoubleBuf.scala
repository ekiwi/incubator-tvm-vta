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

package unittest

import scala.util.Random
import scala.math._

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import vta.core._
import vta.shell._
import vta.util.config._
import ISA._

import unittest.util._



class TestCoreDB(c: Core, commonSize: Int, inpSize: Int, wgtSize: Int, 
                             fctrComm: Int, fctrInp: Int, fctrWgt: Int) extends PeekPokeTester(c) {

  implicit val p = c.p 

  // Run computation
  
  // Matrices to multiply
  //Define matrix size
  
  // ----------------         ----------------         -----------------------
  // |  commonSize  |         |  commonSize  |         |         wgtSize     |
  // |              |         |              |         |                     |
  // |            i |         |            w |         |                   i |
  // |            n |         |            g |         |                   n |
  // |            p |         |            t |         |                   p |
  // |    inp     S |         |    wgt     S |         |           acc     S |
  // |            i |         |            i |         |                   i |
  // |            z |         |            z |         |                   z |
  // |            e |         |            e |         |                   e |
  // |              |         |              |         -----------------------
  // ----------------         |              |        
  //                          |              |        
  //                          |              |        
  //                          ----------------         
  //Inner index goes along the commonSize side

  val batch = c.p(CoreKey).batch
  val wgtTzrSize = c.p(CoreKey).blockOut
  val commonTzrSize = c.p(CoreKey).blockIn
  val bitSize = 2 // data bitwidth, just for smaller numbers to print

  println(s"-I- Running Core batch=${batch} blockOut=${wgtTzrSize} blockIn=${commonTzrSize}")
  
  val memManager = new GeneralMemRead (tester = this, "MemTrace.json")
  //Fill with data
  val r = new Random
  val matrixA = Array.fill(inpSize * commonSize) { r.nextInt(pow(2, bitSize-1).toInt) }
  val inpTen = memManager.tensorize(matrixA, commonTzrSize, batch,  commonSize)
  val matrixB = Array.fill(wgtSize * commonSize) { r.nextInt(pow(2, bitSize-1).toInt) }
  //val wgtTen = tensorize(tensorize(matrixB, blockIn, blockIn, commonSize), // make tensors a continous blocks 
  //                       blockIn * blockIn, wgtSize / blockIn,  commonSize * blockIn) // transpose , same as tr=true
  val wgtTen = memManager.tensorize(matrixB, commonTzrSize, wgtTzrSize, commonSize)
  val tnzrOut  = Array.fill(inpSize * wgtSize) {0}
  val matrixAcc  = Array.fill(inpSize * wgtSize * 4) {0}
  val accTen = memManager.tensorize(matrixAcc, wgtTzrSize, batch,  wgtSize)
  val uops   = Array.fill(p(CoreKey).uopBits/8 * 1024) {0}
  val instr  = Array.fill(16 * 1024) {0}
  
  
  // (<vme channel index>,<name>)
  val readers = List((0,"fetch"),(1,"uop"),(2,"inp"),(3,"wgt"),(4,"acc"))
  val writers = List((0,"store"))
  
  
  val readersState = for((idx,name) <- readers) yield {
    new  DRAMReadStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.rd(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.rd(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.rd(idx).data.bits.data, /* chisel name to poke data */
                                        dataTagSignal = c.io.vme.rd(idx).data.bits.tag, /* chisel name to poke data */
                                        dataLstSignal = c.io.vme.rd(idx).data.bits.last, /* comes with the last pulse*/
                                        validIdxSignal = c.io.vme.rd(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyIdxSignal = c.io.vme.rd(idx).cmd.ready, /* chisel name to poke data valid */
                                        idxSignal = c.io.vme.rd(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.rd(idx).cmd.bits.len, /* chisel name to peek */
                                        tagSignal = c.io.vme.rd(idx).cmd.bits.tag, /* chisel name to peek */
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }
  val writersState = for((idx,name) <- writers) yield {
    new  DRAMWriteStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.wr(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.wr(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.wr(idx).data.bits.data, /* chisel name to peek data */
                                        strbSignal = c.io.vme.wr(idx).data.bits.strb, /* chisel name to peek data */
                                        validCmdSignal = c.io.vme.wr(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyCmdSignal = c.io.vme.wr(idx).cmd.ready, /* chisel name to poke data valid */
                                        dataCmdSignal = c.io.vme.wr(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.wr(idx).cmd.bits.len, /* chisel name to peek index*/
                                        ackSignal = c.io.vme.wr(idx).ack, /* chisel name to poke ack*/
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }
  
  runCompute()
  memManager.end()

  // compare with software matrix multiplication
  // tensorize output to match vta output
  val refResult = memManager.tensorize(memManager.gemmRef(matrixA, matrixB, matrixAcc, commonSize, inpSize, wgtSize), wgtTzrSize, batch,  wgtSize)
  for (idx1 <- 0 until inpSize) {
      for (idx2 <- 0 until wgtSize) {
        require(tnzrOut(idx1 * wgtSize+ idx2) == refResult(idx1 * wgtSize+ idx2),
            s"-F- GEMM failed golden($idx1,$idx2)" +
            s"=${refResult(idx1 * wgtSize+ idx2)} != ${tnzrOut(idx1 * wgtSize+ idx2)}")
      }
    }
  
  
  def runCompute () = {
  

    val instCount = genInstr()
    println(s"-D- inst count=${instCount}")
    
    poke(c.io.vcr.launch, 1)
    poke(c.io.vcr.vals(0), instCount) // how many instructions to read
    poke(c.io.vcr.ptrs(0), 0) // baddr instr
    poke(c.io.vcr.ptrs(1), 0) // baddr uop
    poke(c.io.vcr.ptrs(2), 0) // baddr inp
    poke(c.io.vcr.ptrs(3), 0) // baddr wgt
    poke(c.io.vcr.ptrs(4), 0) // baddr acc
    poke(c.io.vcr.ptrs(5), 0) // wr baddr store
    step(1)
    var clk = 1
    var doCycle = true 
    while (doCycle) {
      clk += 1
      step(1)
      //println(s"Step $clk")
      memManager.readDram(instr, readersState(0)) 
      memManager.readDram(uops, readersState(1)) 
      memManager.readDram(inpTen, readersState(2)) 
      memManager.readDram(wgtTen, readersState(3)) 
      memManager.readDram(accTen, readersState(4)) 
      memManager.writeDram(tnzrOut, writersState(0)) 
      
      if(peek(c.io.vcr.finish) > 0) {
        doCycle = false
      }
      if (clk > 500000) {
        require(false, "-F- Too many iterations")
      }
      //if (clk > 400) {
      //  doCycle = false
      //}
    }
  }
   
   // Instructions ordering:
  // store - any store op
  // load - load op to wgt or inp
  // compute -- all others
  // g2s  do store
  // s2g  do compute
  // g2l  do load
  // l2g  do compute
  // compute push_next g2s ++
  // store pop_prev g2s--
  // store push_prev s2g ++
  // compute pop_next s2g --
  // compute push_prev g2l ++
  // load pop_next g2l --
  // load push_next l2g ++
  // compute pop_prev l2g --

  def genInstr() = {
    val accBegin = 0 // 1st tensor index in scratchpad
    val inpBegin = 0 // 1st tensor index in scratchpad
    val wgtBegin = 0 // 1st tensor index in scratchpad
    val uopBegin = 0 // 1st tensor index in scratchpad
    
    
    val inpTSizeTop = ceil(inpSize / batch.toFloat).toInt // the number of tensors along inp size
    val commonTSizeTop = ceil(commonSize / commonTzrSize.toFloat).toInt // the number of tensors along common size
    val wgtTSizeTop = ceil(wgtSize / wgtTzrSize.toFloat).toInt // the number of tensors along wgt size
    val factorCommon = fctrComm // load 1 column of tensors. need grouping support or a stride read to support other values
    val factorWgt = fctrWgt // load this part of wgt with a single load
    val factorInp = fctrInp // load this part of inp with a single load
    require(commonTSizeTop%factorCommon == 0, "-F- Expecting common split in equal blocks")
    require(wgtTSizeTop%factorWgt == 0, "-F- Expecting wgt split in equal blocks")
    require(inpTSizeTop%factorInp == 0, "-F- Expecting inp split in equal blocks")

    val wgtTSize = wgtTSizeTop / factorWgt  // the number of tensors along wgt size in a single read
    val commonTSize = commonTSizeTop / factorCommon// the number of tensors along common size in a single read
    val inpTSize = inpTSizeTop / factorInp// the number of tensors along inp size in a single read
    val instBytes = INST_BITS/8
    val uopBytes = p(CoreKey).uopBits/8

    var instCnt = 0
    var uopCnt  = 0
    // Reset Acc UOP
    for(uopIdx <- 0 until 2/* double buffer */) {memManager.addAsBytes(uops, memIdx = (uopCnt+uopIdx) * uopBytes, dataBytes = uopBytes, 
                                             memManager.uop(accBegin + wgtTSize * inpTSize * uopIdx, /* acc buffer tensor offset */
                                                            inpBegin /* doesnt matter as we reset*/, 
                                                            wgtBegin /* doesnt matter as we reset*/))}
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = 0,
                   xsize = 2,
                   ysize = 1,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = uopCnt, //uop_baddr
                   sram_offset = uopBegin + uopCnt, // scratchpad idx
                   id = memManager.memId("uop"), 
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 0,
                   pop_prev = 0,
                   op = memManager.taskId("load") // load
                   ))
    instCnt += 1
    uopCnt  += 2 // 2 is just for alignment
    
    val inpClusterSize = commonTSize * inpTSize // the number of tensors to process
    val wgtClusterSize = commonTSize * wgtTSize // the number of tensors to process
    val outClusterSize = inpTSize * wgtTSize // the number of tensors to process
    
    var bufferInZero = true
    var bufferOutZero = true
    for(inpBlIdx <- 0 until factorInp) {
      val inpMemOffset = inpBlIdx * commonTSizeTop * inpTSize
      for(wgtBlIdx <- 0 until factorWgt) {
        // reset accum GEMM instr
        val resetUopIdx = if (bufferOutZero) 0 else 1
        memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.gemInst(
                   empty_1 = 0,
                   wgt_1 = 0,      
                   wgt_0 = 1,      
                   inp_1 = 1,      
                   inp_0 = 0,      
                   acc_1 = 1,     
                   acc_0 = wgtTSize,       
                   empty_0 = 0,     
                   lp_1 = wgtTSize,      
                   lp_0 = inpTSize,      
                   uop_end = 1 + resetUopIdx, 
                   uop_begin = resetUopIdx, 
                   reset = 1,          
                   push_next = 0, // delay next instruction to wait this one to finish
                   push_prev = 0,   
                   //wait for zero buffer store to complete, skip first 2 calls
                   pop_next = if(inpBlIdx * factorWgt + wgtBlIdx > 1) 1 else 0,    
                   pop_prev = 0,    
                   op = memManager.taskId("gemm") 
                   ))
        instCnt += 1
        val wgtMemOffset = wgtBlIdx * commonTSizeTop * wgtTSize
        // run along common direction. 
        val outTensorScrOffset = if(bufferOutZero) 0 else wgtTSize * inpTSize
        for(comBlIdx <- 0 until factorCommon) {
          val inpScrOffset = if(bufferInZero) 0 else commonTSize * inpTSize
          val inpTensorMemOffset = inpMemOffset + comBlIdx * commonTSize
          val inpTensorScrOffset = inpScrOffset //+ comBlIdx * commonTSize * inpTSize
          memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                         xpad_1 = 0,
                         xpad_0 = 0,
                         ypad_1 = 0,
                         ypad_0 = 0,
                         xstride = commonTSizeTop,
                         xsize = commonTSize,
                         ysize = inpTSize,
                         empty_0 = 0,
                         is_min_pad_value = 0,
                         dram_offset = inpTensorMemOffset, // inp column 
                         sram_offset = inpBegin + inpTensorScrOffset, // scratchpad idx
                         id = memManager.memId("inp"), 
                         push_next = 0,
                         push_prev = 0,
                         // wait comp to complete
                         pop_next = if ((inpBlIdx * factorWgt + wgtBlIdx) * factorCommon + comBlIdx > 1) 1 else 0,
                         pop_prev = 0,
                         op = memManager.taskId("load") // load
                         ))
          instCnt += 1
          val wgtScrOffset = if(bufferInZero) 0 else  commonTSize * wgtTSize
          val wgtTensorMemOffset = wgtMemOffset + comBlIdx * commonTSize
          val wgtTensorScrOffset = wgtScrOffset //+ comBlIdx * commonTSize * wgtTSize
          //println(s"-D- wgtTensorMemOffset=${wgtTensorMemOffset} wgtTensorScrOffset=${wgtTensorScrOffset} wgtClusterSize=${wgtClusterSize}")
          //println(s"-D- inpTensorMemOffset=${inpTensorMemOffset} inpTensorScrOffset=${inpTensorScrOffset} inpClusterSize=${inpClusterSize} ")
          //println(s"-D- outTensorScrOffset=${outTensorScrOffset} outClusterSize=${outClusterSize}")
          memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                         xpad_1 = 0,
                         xpad_0 = 0,
                         ypad_1 = 0,
                         ypad_0 = 0,
                         xstride = commonTSizeTop,
                         xsize = commonTSize,
                         ysize = wgtTSize,
                         empty_0 = 0,
                         is_min_pad_value = 0,
                         dram_offset = wgtTensorMemOffset, // wgt column
                         sram_offset = wgtBegin + wgtTensorScrOffset, // scratchpad idx
                         id = memManager.memId("wgt"), 
                         push_next = 1,
                         push_prev = 0,
                         pop_next = 0,
                         pop_prev = 0,
                         op = memManager.taskId("load") // load
                         ))
          instCnt += 1
          //println(s"-D- GEMM parameters: inpTsize=${inpTSize} commonTsize=${commonTSize} wgtTsize=${wgtTSize}")
          for(uopIdx <- 0 until commonTSize) {memManager.addAsBytes(uops, memIdx = (uopCnt + uopIdx)*uopBytes, dataBytes = uopBytes, 
                                memManager.uop(accBegin + outTensorScrOffset,
                                              inpBegin + inpTensorScrOffset + uopIdx, 
                                              wgtBegin + wgtTensorScrOffset + uopIdx))}
          memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                         xpad_1 = 0,
                         xpad_0 = 0,
                         ypad_1 = 0,
                         ypad_0 = 0,
                         xstride = 0,
                         xsize = commonTSize,
                         ysize = 1,
                         empty_0 = 0,
                         is_min_pad_value = 0,
                         dram_offset = uopCnt, //uop_baddr
                         sram_offset = uopBegin + uopCnt, // scratchpad idx
                         id = memManager.memId("uop"), 
                         push_next = 0,
                         push_prev = 0,
                         pop_next = 0,
                         pop_prev = 0,
                         op = memManager.taskId("load") // load
                         ))
          instCnt += 1
          memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.gemInst(
                         empty_1 = 0,
                         wgt_1 = 0,      
                         wgt_0 = commonTSize,      
                         inp_1 = commonTSize,      
                         inp_0 = 0,      
                         acc_1 = wgtTSize,     
                         acc_0 = 1,       
                         empty_0 = 0,     
                         lp_1 = inpTSize,      
                         lp_0 = wgtTSize,      
                         uop_end = uopBegin + uopCnt + commonTSize, 
                         uop_begin = uopBegin + uopCnt, 
                         reset = 0,          
                         push_next = if(comBlIdx == factorCommon -1) 1 else 0, // delay next instruction to wait this one to finish
                         // deleay load to this input buffer
                         push_prev = if((inpBlIdx * factorWgt + wgtBlIdx) * factorCommon + comBlIdx < factorInp * factorWgt * factorCommon - 2) 1 else 0,   
                         pop_next = 0,    
                         pop_prev = 1,    
                         op = memManager.taskId("gemm") 
                         ))
          uopCnt += commonTSize                    
          instCnt += 1
          bufferInZero = !bufferInZero
        }
        // store out 
        memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                     xpad_1 = 0,
                     xpad_0 = 0,
                     ypad_1 = 0,
                     ypad_0 = 0,
                     xstride = wgtTSizeTop,
                     xsize = wgtTSize,
                     ysize = inpTSize,
                     empty_0 = 0,
                     is_min_pad_value = 0,
                     dram_offset = inpBlIdx * wgtTSizeTop * inpTSize + wgtBlIdx * wgtTSize, 
                     sram_offset = accBegin + outTensorScrOffset, // scratchpad idx
                     id = memManager.memId("wgt"), // not used
                     push_next = 0,
                     // block compute inot the same out
                     push_prev = if (inpBlIdx * factorWgt + wgtBlIdx < factorInp * factorWgt - 2) 1 else 0, 
                     pop_next = 0,
                     pop_prev = 1, // wait gemm instruction to finish
                     op = memManager.taskId("store") // store
                     ))
        instCnt += 1
        bufferOutZero = !bufferOutZero
      }
    }

    // finish
    // sync store
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                 xpad_1 = 0,
                 xpad_0 = 0,
                 ypad_1 = 0,
                 ypad_0 = 0,
                 xstride = 0,
                 xsize = 0, // sync store
                 ysize = 0,
                 empty_0 = 0,
                 is_min_pad_value = 0,
                 dram_offset = 0, 
                 sram_offset = 0, // scratchpad idx
                 id = memManager.memId("wgt"), // not used
                 push_next = 0,
                 // block buffer zero compute and last compute
                 push_prev = 1, 
                 pop_next = 0,
                 pop_prev = 0, // runs after store
                 op = memManager.taskId("store") // store
                 ))
    instCnt += 1
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.finInst(
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 1,
                   pop_prev = 0, 
                   op = memManager.taskId("finish") // finish
                   ))
    instCnt += 1
    instCnt
  }
}
class TestCoreDBcont(c: Core, commonSize: Int, inpSize: Int, wgtSize: Int, 
                             fctrComm: Int, fctrInp: Int, fctrWgt: Int) extends PeekPokeTester(c) {

  implicit val p = c.p 

  // Run computation
  
  // Matrices to multiply
  //Define matrix size
  
  // ----------------         ----------------         -----------------------
  // |  commonSize  |         |  commonSize  |         |         wgtSize     |
  // |              |         |              |         |                     |
  // |            i |         |            w |         |                   i |
  // |            n |         |            g |         |                   n |
  // |            p |         |            t |         |                   p |
  // |    inp     S |         |    wgt     S |         |           acc     S |
  // |            i |         |            i |         |                   i |
  // |            z |         |            z |         |                   z |
  // |            e |         |            e |         |                   e |
  // |              |         |              |         -----------------------
  // ----------------         |              |        
  //                          |              |        
  //                          |              |        
  //                          ----------------         
  //Inner index goes along the commonSize side

  val batch = c.p(CoreKey).batch
  val wgtTzrSize = c.p(CoreKey).blockOut
  val commonTzrSize = c.p(CoreKey).blockIn
  val bitSize = 2 // data bitwidth, just for smaller numbers to print

  println(s"-I- Running Core batch=${batch} blockOut=${wgtTzrSize} blockIn=${commonTzrSize}. Length uop=${p(CoreKey).uopMemDepth} tensors inp=${p(CoreKey).inpMemDepth} wgt=${p(CoreKey).wgtMemDepth} acc=${p(CoreKey).accMemDepth}")
  
  val memManager = new GeneralMemRead (tester = this, s"MemTrace.json")
  
  //Fill with data
  val r = new Random
  val matrixA = Array.fill(inpSize * commonSize) { r.nextInt(pow(2, bitSize-1).toInt) }
  val inpTen = memManager.tensorize(memManager.tensorize(matrixA, commonTzrSize, batch,  commonSize), // serialize tensors
                          commonSize/fctrComm*batch, inpSize/fctrInp/batch, commonSize * batch) // serialize clusters
  val matrixB = Array.fill(wgtSize * commonSize) { r.nextInt(pow(2, bitSize-1).toInt) }
  val wgtTen = memManager.tensorize(memManager.tensorize(matrixB, commonTzrSize, wgtTzrSize, commonSize), // serialize tensors
                          commonSize/fctrComm*wgtTzrSize, wgtSize/fctrWgt/wgtTzrSize, commonSize * wgtTzrSize) // serialize clusters
  val tnzrOut  = Array.fill(inpSize * wgtSize) {0}
  val matrixAcc  = Array.fill(inpSize * wgtSize * 4) {0}
  val accTen = memManager.tensorize(memManager.tensorize(matrixAcc, wgtTzrSize, batch,  wgtSize),
                          wgtSize/fctrWgt*batch, inpSize/fctrInp/batch, wgtSize * batch) // serialize clusters
  val uops   = Array.fill(p(CoreKey).uopBits/8 * p(CoreKey).uopMemDepth) {0}
  //val instr  = Array.fill(16 * 1024) {0}
  //val instr  = Array.fill(16 * pow(2, c.p(ShellKey).memParams.lenBits).toInt) {0}
  val instr  = Array.fill(2024*1024) {0}
  
  // Align adress to tensor/uop size as dram_offset is in respective units 
  val wgtBeginDram = 0
  val inpBeginDram = ceil((wgtBeginDram + wgtTen.length)/(batch*commonTzrSize).toFloat).toInt
  val accBeginDram = ceil((inpBeginDram*batch*commonTzrSize + inpTen.length)/(wgtTzrSize*batch*4).toFloat).toInt
  val strBeginDram = ceil((accBeginDram*wgtTzrSize*batch*4 + accTen.length)/(wgtTzrSize*batch).toFloat).toInt
  val uopBeginDram = ceil((strBeginDram*wgtTzrSize*batch + tnzrOut.length)/(c.p(CoreKey).uopBits/8).toFloat).toInt
  val insBeginDram = uopBeginDram*(c.p(CoreKey).uopBits/8) + uops.length
  val memory = Array.fill(insBeginDram + instr.length){0}
  Array.copy(wgtTen, 0, memory, wgtBeginDram*wgtTzrSize*commonTzrSize, wgtTen.length)
  Array.copy(inpTen, 0, memory, inpBeginDram*batch*commonTzrSize, inpTen.length)
  Array.copy(accTen, 0, memory, accBeginDram*wgtTzrSize*batch*4, accTen.length)
  println(s"-D-  baddr: \n wgtBeginDram ${BigInt(wgtBeginDram).toString(2)}:${wgtTen.length} \n" +
          s" inpBeginDram ${BigInt(inpBeginDram).toString(10)}:${inpTen.length} \n" +
          s" accBeginDram ${BigInt(accBeginDram).toString(10)}:${accTen.length} \n" +
          s" strBeginDram ${BigInt(strBeginDram).toString(10)}:${tnzrOut.length} \n" +
          s" uopBeginDram ${BigInt(uopBeginDram).toString(10)}:${uops.length} \n" +
          s" insBeginDram ${BigInt(insBeginDram).toString(10)}:${instr.length}"
         )
  
  //val (instr, offsetT)  = memManager.readDump("mem.json")
  // (<vme channel index>,<name>)
  val readers = List((0,"fetch"),(1,"uop"),(2,"inp"),(3,"wgt"),(4,"acc"))
  val writers = List((0,"store"))
  
  
  val readersState = for((idx,name) <- readers) yield {
    new  DRAMReadStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.rd(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.rd(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.rd(idx).data.bits.data, /* chisel name to poke data */
                                        dataTagSignal = c.io.vme.rd(idx).data.bits.tag, /* chisel name to poke data */
                                        dataLstSignal = c.io.vme.rd(idx).data.bits.last, /* comes with the last pulse*/
                                        validIdxSignal = c.io.vme.rd(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyIdxSignal = c.io.vme.rd(idx).cmd.ready, /* chisel name to poke data valid */
                                        idxSignal = c.io.vme.rd(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.rd(idx).cmd.bits.len, /* chisel name to peek index*/
                                        tagSignal = c.io.vme.rd(idx).cmd.bits.tag, /* chisel name to peek index*/
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }
  val writersState = for((idx,name) <- writers) yield {
    new  DRAMWriteStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.wr(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.wr(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.wr(idx).data.bits.data, /* chisel name to peek data */
                                        strbSignal = c.io.vme.wr(idx).data.bits.strb, /* chisel name to peek data */
                                        validCmdSignal = c.io.vme.wr(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyCmdSignal = c.io.vme.wr(idx).cmd.ready, /* chisel name to poke data valid */
                                        dataCmdSignal = c.io.vme.wr(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.wr(idx).cmd.bits.len, /* chisel name to peek index*/
                                        ackSignal = c.io.vme.wr(idx).ack, /* chisel name to poke ack*/
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }
  
  runCompute()
  memManager.end()
  
  // compare with software matrix multiplication
  // tensorize output to match vta output
  val refResult = memManager.tensorize(memManager.tensorize(memManager.gemmRef(matrixA, matrixB, matrixAcc, commonSize, inpSize, wgtSize)
                                                            , wgtTzrSize, batch,  wgtSize),
                                       wgtSize/fctrWgt*batch, inpSize/fctrInp/batch, wgtSize * batch) // serialize clusters
  for (idx1 <- 0 until inpSize) {
      for (idx2 <- 0 until wgtSize) {
        require(memory(strBeginDram*wgtTzrSize*batch + idx1 * wgtSize+ idx2) == refResult(idx1 * wgtSize+ idx2),
            s"-F- GEMM failed golden($idx1,$idx2)" +
            s"=${refResult(idx1 * wgtSize+ idx2)} != ${tnzrOut(idx1 * wgtSize+ idx2)}")
      }
    }
  
  
  def runCompute () = {
  

    val instCount = genInstr()
    println(s"-D- inst count=${instCount}")
    
    poke(c.io.vcr.launch, 1)
    poke(c.io.vcr.vals(0), instCount) // how many instructions to read
    poke(c.io.vcr.ptrs(0), insBeginDram) // baddr instr
    poke(c.io.vcr.ptrs(1), 0) // baddr uop
    poke(c.io.vcr.ptrs(2), 0) // baddr inp
    poke(c.io.vcr.ptrs(3), 0) // baddr wgt
    poke(c.io.vcr.ptrs(4), 0) // baddr acc
    poke(c.io.vcr.ptrs(5), 0) // wr baddr store
    step(1)
    var clk = 1
    var doCycle = true 
    while (doCycle) {
      clk += 1
      step(1)
      //println(s"Step $clk")
      memManager.readDram(memory, readersState(0)) 
      memManager.readDram(memory, readersState(1)) 
      memManager.readDram(memory, readersState(2)) 
      memManager.readDram(memory, readersState(3)) 
      memManager.readDram(memory, readersState(4)) 
      memManager.writeDram(memory, writersState(0)) 
      
      if(peek(c.io.vcr.finish) > 0) {
        doCycle = false
      }
      if (clk > 500000) {
        require(false, "-F- Too many iterations")
      }
      //if (clk > 400) {
      //  doCycle = false
      //}
    }
  }
   
   // Instructions ordering:
  // store - any store op
  // load - load op to wgt or inp
  // compute -- all others
  // g2s  do store
  // s2g  do compute
  // g2l  do load
  // l2g  do compute
  // compute push_next g2s ++
  // store pop_prev g2s--
  // store push_prev s2g ++
  // compute pop_next s2g --
  // compute push_prev g2l ++
  // load pop_next g2l --
  // load push_next l2g ++
  // compute pop_prev l2g --

  def genInstr() = {
    val accBegin = 0 // 1st tensor index in scratchpad
    val inpBegin = 0 // 1st tensor index in scratchpad
    val wgtBegin = 0 // 1st tensor index in scratchpad
    val uopBegin = 0 // 1st tensor index in scratchpad
    
    
    val inpTSizeTop = ceil(inpSize / batch.toFloat).toInt // the number of tensors along inp size
    val commonTSizeTop = ceil(commonSize / commonTzrSize.toFloat).toInt // the number of tensors along common size
    val wgtTSizeTop = ceil(wgtSize / wgtTzrSize.toFloat).toInt // the number of tensors along wgt size
    val factorCommon = fctrComm // load 1 column of tensors. need grouping support or a stride read to support other values
    val factorWgt = fctrWgt // load this part of wgt with a single load
    val factorInp = fctrInp // load this part of inp with a single load
    require(commonTSizeTop%factorCommon == 0, "-F- Expecting common split in equal blocks")
    require(wgtTSizeTop%factorWgt == 0, "-F- Expecting wgt split in equal blocks")
    require(inpTSizeTop%factorInp == 0, "-F- Expecting inp split in equal blocks")

    val wgtTSize = wgtTSizeTop / factorWgt  // the number of tensors along wgt size in a single read
    val commonTSize = commonTSizeTop / factorCommon// the number of tensors along common size in a single read
    val inpTSize = inpTSizeTop / factorInp// the number of tensors along inp size in a single read
    val instBytes = INST_BITS/8
    val uopBytes = p(CoreKey).uopBits/8

    val inpClusterSize = commonTSize * inpTSize // the number of tensors to process
    val wgtClusterSize = commonTSize * wgtTSize // the number of tensors to process
    val outClusterSize = inpTSize * wgtTSize // the number of tensors to process

    var instCnt = 0
    val bufferZeroZeroUOPIdx = 0
    val bufferOneZeroUOPIdx  = bufferZeroZeroUOPIdx + commonTSize
    val bufferZeroOneUOPIdx  = bufferOneZeroUOPIdx + commonTSize
    val bufferOneOneUOPIdx   = bufferZeroOneUOPIdx + commonTSize
    require((bufferOneOneUOPIdx + commonTSize - 1) * uopBytes < uops.length, s"-D- Too many uops to fit into uop memory ${(bufferOneOneUOPIdx + commonTSize)} < ${uops.length/uopBytes}")
    for(uopIdx <- 0 until commonTSize) {
      //gemm buffer in 0 out 0
      memManager.addAsBytes(memory, memIdx = uopBeginDram*uopBytes + (bufferZeroZeroUOPIdx + uopIdx)*uopBytes, dataBytes = uopBytes, 
                           memManager.uop(accBegin,
                                          inpBegin + uopIdx, 
                                          wgtBegin + uopIdx))
      //gemm buffer in 0 out 1
      memManager.addAsBytes(memory, memIdx = uopBeginDram*uopBytes + (bufferZeroOneUOPIdx + uopIdx)*uopBytes, dataBytes = uopBytes, 
                           memManager.uop(accBegin + outClusterSize,
                                          inpBegin + uopIdx, 
                                          wgtBegin + uopIdx))
      //gemm buffer in 1 out 0
      memManager.addAsBytes(memory, memIdx = uopBeginDram*uopBytes + (bufferOneZeroUOPIdx + uopIdx)*uopBytes, dataBytes = uopBytes, 
                           memManager.uop(accBegin,
                                          inpBegin + inpClusterSize + uopIdx, 
                                          wgtBegin + wgtClusterSize + uopIdx))
      //gemm buffer in 1 out 1
      memManager.addAsBytes(memory, memIdx = uopBeginDram*uopBytes + (bufferOneOneUOPIdx + uopIdx)*uopBytes, dataBytes = uopBytes, 
                           memManager.uop(accBegin + outClusterSize,
                                          inpBegin + inpClusterSize + uopIdx, 
                                          wgtBegin + wgtClusterSize + uopIdx))
    }
    val uopCnt  = bufferOneOneUOPIdx + commonTSize
    memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = uopCnt,
                   xsize = uopCnt,
                   ysize = 1,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = uopBeginDram, //uop_baddr
                   sram_offset = uopBegin, // scratchpad idx
                   id = memManager.memId("uop"), 
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 0,
                   pop_prev = 0,
                   op = memManager.taskId("load") // load
                   ))
    instCnt += 1
    
    println(s"-D- Doing inp x wgt x common ${factorInp}x${factorWgt}x${factorCommon} cluster multiplications. Gemm multiplications ${inpTSize}x${wgtTSize}x${commonTSize}  ")
    
    var bufferInZero = true
    var bufferOutZero = true
    for(inpBlIdx <- 0 until factorInp) {
      val inpMemOffset = inpBlIdx * inpClusterSize * factorCommon
      for(wgtBlIdx <- 0 until factorWgt) {
        // reset accum GEMM instr
        val resetIdx = if (bufferOutZero) bufferZeroZeroUOPIdx else bufferZeroOneUOPIdx
        memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.gemInst(
                   empty_1 = 0,
                   wgt_1 = 0,      
                   wgt_0 = 1,      
                   inp_1 = 1,      
                   inp_0 = 0,      
                   acc_1 = 1,     
                   acc_0 = wgtTSize,       
                   empty_0 = 0,     
                   lp_1 = wgtTSize,      
                   lp_0 = inpTSize,      
                   uop_end = 1 + resetIdx, // dont need whole accumolate sequence. the first oup is enough
                   uop_begin = resetIdx, 
                   reset = 1,          
                   push_next = 0, // delay next instruction to wait this one to finish
                   push_prev = 0,   
                   //wait for zero buffer store to complete, skip first 2 calls
                   pop_next = if(inpBlIdx * factorWgt + wgtBlIdx > 1) 1 else 0,    
                   pop_prev = 0,    
                   op = memManager.taskId("gemm") 
                   ))
        instCnt += 1
        val wgtMemOffset = wgtBlIdx * wgtClusterSize * factorCommon
        // run along common direction. 
        val outTensorScrOffset = if(bufferOutZero) 0 else outClusterSize
        for(comBlIdx <- 0 until factorCommon) {
          val inpScrOffset = if(bufferInZero) 0 else inpClusterSize
          val inpTensorMemOffset = inpMemOffset + comBlIdx * inpClusterSize
          val inpTensorScrOffset = inpScrOffset //+ comBlIdx * commonTSize * inpTSize
          memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                         xpad_1 = 0,
                         xpad_0 = 0,
                         ypad_1 = 0,
                         ypad_0 = 0,
                         xstride = commonTSize*inpTSize,
                         xsize = commonTSize*inpTSize,
                         ysize = 1,
                         empty_0 = 0,
                         is_min_pad_value = 0,
                         dram_offset = inpBeginDram + inpTensorMemOffset, // inp column 
                         sram_offset = inpBegin + inpTensorScrOffset, // scratchpad idx
                         id = memManager.memId("inp"), 
                         push_next = 0,
                         push_prev = 0,
                         // wait comp to complete
                         pop_next = if ((inpBlIdx * factorWgt + wgtBlIdx) * factorCommon + comBlIdx > 1) 1 else 0,
                         pop_prev = 0,
                         op = memManager.taskId("load") // load
                         ))
          instCnt += 1
          val wgtScrOffset = if(bufferInZero) 0 else  wgtClusterSize
          val wgtTensorMemOffset = wgtMemOffset + comBlIdx * wgtClusterSize
          val wgtTensorScrOffset = wgtScrOffset //+ comBlIdx * commonTSize * wgtTSize
          //println(s"-D- wgtTensorMemOffset=${wgtTensorMemOffset} wgtTensorScrOffset=${wgtTensorScrOffset} wgtClusterSize=${wgtClusterSize}")
          //println(s"-D- inpTensorMemOffset=${inpTensorMemOffset} inpTensorScrOffset=${inpTensorScrOffset} inpClusterSize=${inpClusterSize} ")
          //println(s"-D- outTensorScrOffset=${outTensorScrOffset} outClusterSize=${outClusterSize}")
          memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                         xpad_1 = 0,
                         xpad_0 = 0,
                         ypad_1 = 0,
                         ypad_0 = 0,
                         xstride = commonTSize*wgtTSize,
                         xsize = commonTSize*wgtTSize,
                         ysize = 1,
                         empty_0 = 0,
                         is_min_pad_value = 0,
                         dram_offset = wgtBeginDram + wgtTensorMemOffset, // wgt column
                         sram_offset = wgtBegin + wgtTensorScrOffset, // scratchpad idx
                         id = memManager.memId("wgt"), 
                         push_next = 1,
                         push_prev = 0,
                         pop_next = 0,
                         pop_prev = 0,
                         op = memManager.taskId("load") // load
                         ))
          instCnt += 1
          //println(s"-D- GEMM parameters: inpTsize=${inpTSize} commonTsize=${commonTSize} wgtTsize=${wgtTSize}")
          val uopOffset = if(bufferInZero) {
              if (bufferOutZero) bufferZeroZeroUOPIdx else bufferZeroOneUOPIdx
            } else {
              if (bufferOutZero) bufferOneZeroUOPIdx else bufferOneOneUOPIdx
            }
          memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.gemInst(
                         empty_1 = 0,
                         wgt_1 = 0,      
                         wgt_0 = commonTSize,      
                         inp_1 = commonTSize,      
                         inp_0 = 0,      
                         acc_1 = wgtTSize,     
                         acc_0 = 1,       
                         empty_0 = 0,     
                         lp_1 = inpTSize,      
                         lp_0 = wgtTSize,      
                         uop_end = uopBegin + uopOffset + commonTSize, 
                         uop_begin = uopBegin + uopOffset, 
                         reset = 0,          
                         push_next = if(comBlIdx == factorCommon -1) 1 else 0, // delay next instruction to wait this one to finish
                         // deleay load to this input buffer
                         push_prev = if((inpBlIdx * factorWgt + wgtBlIdx) * factorCommon + comBlIdx < factorInp * factorWgt * factorCommon - 2) 1 else 0,   
                         pop_next = 0,    
                         pop_prev = 1,    
                         op = memManager.taskId("gemm") 
                         ))
          instCnt += 1
          bufferInZero = !bufferInZero
        }
        // store out 
        memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                     xpad_1 = 0,
                     xpad_0 = 0,
                     ypad_1 = 0,
                     ypad_0 = 0,
                     xstride = outClusterSize,
                     xsize = outClusterSize,
                     ysize = 1,
                     empty_0 = 0,
                     is_min_pad_value = 0,
                     dram_offset = strBeginDram + outClusterSize *(inpBlIdx * factorWgt + wgtBlIdx), 
                     sram_offset = accBegin + outTensorScrOffset, // scratchpad idx
                     id = memManager.memId("wgt"), // not used
                     push_next = 0,
                     // block compute inot the same out
                     push_prev = if (inpBlIdx * factorWgt + wgtBlIdx < factorInp * factorWgt - 2) 1 else 0, 
                     pop_next = 0,
                     pop_prev = 1, // wait gemm instruction to finish
                     op = memManager.taskId("store") // store
                     ))
        instCnt += 1
        bufferOutZero = !bufferOutZero
      }
    }

    // finish
    // sync store
    memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                 xpad_1 = 0,
                 xpad_0 = 0,
                 ypad_1 = 0,
                 ypad_0 = 0,
                 xstride = 0,
                 xsize = 0, // sync store
                 ysize = 0,
                 empty_0 = 0,
                 is_min_pad_value = 0,
                 dram_offset = 0, 
                 sram_offset = 0, // scratchpad idx
                 id = memManager.memId("wgt"), // not used
                 push_next = 0,
                 // block buffer zero compute and last compute
                 push_prev = 1, 
                 pop_next = 0,
                 pop_prev = 0, // runs after store
                 op = memManager.taskId("store") // store
                 ))
    instCnt += 1
    memManager.addAsBytes(memory, memIdx = insBeginDram + instBytes*instCnt, dataBytes = instBytes, memManager.finInst(
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 1,
                   pop_prev = 0, 
                   op = memManager.taskId("finish") // finish
                   ))
    instCnt += 1
    memManager.setStart(insBeginDram, instCnt)
    instCnt
  }
}
class TestCoreFromFile(c: Core, dumpfileName: String) extends PeekPokeTester(c) {

  implicit val p = c.p 

  val batch = c.p(CoreKey).batch
  val blockOut = c.p(CoreKey).blockOut
  val blockIn = c.p(CoreKey).blockIn

  println(s"-I- Running Core batch=${batch} blockOut=${blockOut} blockIn=${blockIn}")
  
  val memManager = new GeneralMemRead (tester = this, "debug.json")
  
  
  val readers = List((0,"fetch"),(1,"uop"),(2,"inp"),(3,"wgt"),(4,"acc"))
  val writers = List((0,"store"))
  val readersState = for((idx,name) <- readers) yield {
    new  DRAMReadStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.rd(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.rd(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.rd(idx).data.bits.data, /* chisel name to poke data */
                                        dataTagSignal = c.io.vme.rd(idx).data.bits.tag, /* chisel name to poke data */
                                        validIdxSignal = c.io.vme.rd(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyIdxSignal = c.io.vme.rd(idx).cmd.ready, /* chisel name to poke data valid */
                                        idxSignal = c.io.vme.rd(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.rd(idx).cmd.bits.len, /* chisel name to peek index*/
                                        tagSignal = c.io.vme.rd(idx).cmd.bits.tag, /* chisel name to peek index*/
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }
  val writersState = for((idx,name) <- writers) yield {
    new  DRAMWriteStateData (name = name,   /* memory name for printing */ 
                                        validDataSignal = c.io.vme.wr(idx).data.valid, /* chisel name to poke data valid */
                                        readyDataSignal = c.io.vme.wr(idx).data.ready, /* chisel name to poke data valid */
                                        dataSignal = c.io.vme.wr(idx).data.bits.data, /* chisel name to poke data */
                                        strbSignal = c.io.vme.wr(idx).data.bits.strb, /* chisel name to poke data */
                                        validCmdSignal = c.io.vme.wr(idx).cmd.valid, /* chisel name to peek index valid */
                                        readyCmdSignal = c.io.vme.wr(idx).cmd.ready, /* chisel name to poke data valid */
                                        dataCmdSignal = c.io.vme.wr(idx).cmd.bits.addr, /* chisel name to peek index*/
                                        lenSignal = c.io.vme.wr(idx).cmd.bits.len, /* chisel name to peek index*/
                                        ackSignal = c.io.vme.wr(idx).ack, /* chisel name to poke ack*/
                                        latency = () => 7 /* chisel name to peek index*/
                                        )  
  }

  runCompute()
  
  //======= end of the test ===========================================
  
  def runCompute () = {
  

    val (memory, insBeginDram, instrCount) = memManager.readDump(dumpfileName)
    memManager.setAssertWrites(true)
    println(s"-D- inst count=${instrCount}")
    
    poke(c.io.vcr.launch, 1)
    poke(c.io.vcr.vals(0), instrCount) // how many instructions to read
    poke(c.io.vcr.ptrs(0), insBeginDram) // baddr instr
    poke(c.io.vcr.ptrs(1), 0) // baddr uop
    poke(c.io.vcr.ptrs(2), 0) // baddr inp
    poke(c.io.vcr.ptrs(3), 0) // baddr wgt
    poke(c.io.vcr.ptrs(4), 0) // baddr acc
    poke(c.io.vcr.ptrs(5), 0) // wr baddr store
    step(1)
    var clk = 1
    var doCycle = true 
    while (doCycle) {
      clk += 1
      step(1)
      //println(s"Step $clk")
      memManager.readDram(memory, readersState(0)) 
      memManager.readDram(memory, readersState(1)) 
      memManager.readDram(memory, readersState(2)) 
      memManager.readDram(memory, readersState(3)) 
      memManager.readDram(memory, readersState(4)) 
      memManager.writeDram(memory, writersState(0)) 
      
      if(peek(c.io.vcr.finish) > 0) {
        doCycle = false
      }
      if (clk > 500000) {
        require(false, "-F- Too many iterations")
      }
      //if (clk > 400) {
      //  doCycle = false
      //}
    }
  }
   
}
class CoreTestDB64x64x64_2_2_2_stride extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDB(c, 64, 64, 64, 2, 2, 2))
class CoreTestDB64x64x64_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 64, 64, 64, 2, 2, 2))
//class CoreTestDB32x256x256_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 32, 256, 256, 2, 2, 2))
//class CoreTestDB32x512x512_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 32, 512, 512, 2, 2, 2))
//class CoreTestDB64x128x128_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 64, 128, 128, 2, 2, 2))
//class CoreTestDB64x128x128_4_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 64, 128, 128, 4, 2, 2))
//class CoreTestDB16x512x512_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 16, 512, 512, 2, 2, 2))
//class CoreTestDB16x256x256_2_2_2_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 16, 256, 256, 2, 2, 2))
//class CoreTestDB512x4x16_2_4_1_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 4, 16, 2, 4, 1))
//class CoreTestDB512x4x16_2_1_1_cont extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 4, 16, 2, 1, 1))
//class CoreTestDB64x64x64_2_2_2_cont_load extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreFromFile(c, "MemTrace64x64x64_2_2_2.json"))
//class CoreTestDB2 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDB(c, 512, 256, 128, 4, 2, 1))
//class CoreTestDBcont2 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 256, 128, 512/16, 1, 2))
//class CoreTestDBcont2pynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 256, 128, 512/16, 1, 2))
//class CoreTestDBcont3pynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 256, 128, 512/16, 1, 2))
//class CoreTestDBcont4pynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 256, 256, 512/16, 1, 1))
//class CoreTestDBcont5pynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 512, 256, 256, 512/16, 2, 2))
//class CoreTestDBcont6pynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 16, 256, 256, 1, 1, 1))
//class CoreTestDB1 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDB(c, 32, 256, 256, 2, 2, 2))
//class CoreTestDBcontPynq extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDBcont(c, 16 * 10, 768, 768, 10, 6, 6))
class CoreTest1 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new TestCoreDB(c, 32, 4, 64, 1, 1, 2))
