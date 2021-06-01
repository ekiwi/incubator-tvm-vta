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
import vta.util.config._
import ISA._

import unittest.util._



class AccLoadTestMax(c: Core) extends AccLoadTest(c, c.p(CoreKey).accMemDepth, c.p(CoreKey).blockOut) {

}
class AccLoadTest(c: Core, inpSize: Int, wgtSize: Int) extends PeekPokeTester(c) {

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
  val blockOut = c.p(CoreKey).blockOut
  val blockIn = c.p(CoreKey).blockIn
  val bitSize = 2 // data bitwidth, just for smaller numbers to print
  val commonSize = wgtSize
  println(s"-I- Running Core batch=${batch} blockOut=${blockOut} blockIn=${blockIn}")
  println(s"-I- Running Core scratchpad sizes: wgt=${p(CoreKey).wgtMemDepth} inp=${p(CoreKey).inpMemDepth} acc=${p(CoreKey).accMemDepth}")
  val memManager = new GeneralMemRead (tester = this)

  val accAddr = 1 // in tensors
  //Fill with data
  val r = new Random
  val tnzrOut  = Array.fill(inpSize * wgtSize) {0}
  val valAcc = Array.fill(inpSize * wgtSize) { r.nextInt(pow(2, bitSize-1).toInt) }
  val matrixAcc = Array.fill(inpSize * wgtSize * 4) {0}
  for (idx <- 0 until inpSize * wgtSize) {matrixAcc(idx*4) = valAcc(idx)}
  //val accTen = memManager.tensorize(matrixAcc, blockIn * 4, batch, wgtSize * 4)
  val accTen = Array.fill(accAddr*batch*blockOut*4 + inpSize * wgtSize * 4) {0}
  for (idx <- 0 until matrixAcc.length) {accTen(idx + accAddr*batch*blockOut*4) = matrixAcc(idx)}
  val instr  = Array.fill(16 * 1024) {0}
  val uops   = Array.fill(p(CoreKey).uopBits/8 * 1024) {0}
  val matrixA = Array.fill(inpSize * commonSize) {0}
  val inpTen = memManager.tensorize(matrixA, blockIn, batch,  commonSize)
  val matrixB = Array.fill(wgtSize * commonSize) {0}
  val wgtTen = memManager.tensorize(matrixB, blockIn, blockOut, commonSize)
  
  
//  println ("-D- lod test Gold:")
//  for (idx1 <- 0 until inpSize) {
//    val start = idx1 * wgtSize
//    val end = start + wgtSize
//    println (s"""-D- acc load test ${valAcc.slice(start, end).mkString(",")} """)
//  }
 
  
  runCompute()

  // compare with software matrix multiplication
  // tensorize output to match vta output
  val refResult = valAcc

//  println ("-D- load test Res:")
//  for (idx1 <- 0 until inpSize) {
//    val start = idx1 * wgtSize
//    val end = start + wgtSize
//    println (s"""-D- acc load test ${tnzrOut.slice(start, end).mkString(",")} """)
//  }

  for (idx1 <- 0 until inpSize) {
      for (idx2 <- 0 until wgtSize) {
        require(tnzrOut(idx1 * wgtSize+ idx2) == refResult(idx1 * wgtSize+ idx2),
            s"-F- GEMM failed golden($idx1,$idx2)" +
            s"=${refResult(idx1 * wgtSize+ idx2)} != ${tnzrOut(idx1 * wgtSize+ idx2)}")
      }
    }
  //--- END OF RUN ---
  
  def runCompute () = {
  

    val (readersState, writersState) = memManager.dutCoreReaderState(c)
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

  // load acc, run gemm with zero inputs, store
  def genInstr() = {
    val accBegin = 0 // 1st tensor index in scratchpad
    val inpBegin = 0 // 1st tensor index in scratchpad
    val wgtBegin = 0 // 1st tensor index in scratchpad
    val uopBegin = 0 // 1st tensor index in scratchpad
    
    
    val inpTSize = ceil(inpSize / batch.toFloat).toInt
    val wgtTSize = ceil(wgtSize / blockOut.toFloat).toInt
    val commonTSize = wgtTSize
    val instBytes = INST_BITS/8
    val uopBytes = p(CoreKey).uopBits/8

    var instCnt = 0
    var uopCnt = 0
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = wgtTSize*inpTSize,
                   xsize = wgtTSize*inpTSize,
                   ysize = 1,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = accAddr, 
                   sram_offset = accBegin, // scratchpad idx
                   id = memManager.memId("acc"), 
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 0,
                   pop_prev = 0,
                   op = memManager.taskId("load") // load
                   ))
    instCnt += 1
    memManager.addAsBytes(uops, memIdx = uopCnt*uopBytes, dataBytes = uopBytes, 
                                             memManager.uop(accBegin,inpBegin,wgtBegin))
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = 1,
                   xsize = 1,
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
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.aluInst( 
               empty_1 = 0,      
               alu_imm = 0,      
               alu_use_imm = 1,      
               alu_op = memManager.aluId("add"),      
               src_1 = 1,     
               src_0 = wgtTSize,       
               dst_1 = 1,     
               dst_0 = wgtTSize, 
               empty_0 = 0, 
               lp_1 = wgtTSize, 
               lp_0 = inpTSize, 
               uop_end = uopBegin + uopCnt + 1,
               uop_begin = uopBegin + uopCnt,
               reset = 0,          
               push_next = 1,   
               push_prev = 0,   
               pop_next = 0,    
               pop_prev = 0,    
               memManager.taskId("alu")          
               ))
    uopCnt += 1                    
    instCnt += 1
    // store out 
    memManager.addAsBytes(instr, memIdx = instBytes*instCnt, dataBytes = instBytes, memManager.memInst(
                 xpad_1 = 0,
                 xpad_0 = 0,
                 ypad_1 = 0,
                 ypad_0 = 0,
                 xstride = wgtTSize*inpTSize,
                 xsize = wgtTSize*inpTSize,
                 ysize = 1,
                 empty_0 = 0,
                 is_min_pad_value = 0,
                 dram_offset = 0, 
                 sram_offset = accBegin, // scratchpad idx
                 id = memManager.memId("wgt"), // not used
                 push_next = 0,
                 push_prev = 1, // bock last compute which is finish
                 pop_next = 0,
                 pop_prev = 1, 
                 op = memManager.taskId("store") // store
                 ))
    instCnt += 1

    // finish
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
class AccLoad64x64 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new AccLoadTest(c, 8, 32))
class AccLoad4x256 extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new AccLoadTest(c, 4, 256))
//class AccLoadMax extends GenericTest( "Core", (p:Parameters) => new Core()(p), (c:Core) => new AccLoadTestMax(c))
