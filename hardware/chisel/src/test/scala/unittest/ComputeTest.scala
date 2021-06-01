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
import scala.math.pow

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import vta.core._
import vta.util.config._
import ISA._

import unittest.util._



class TestCompute(c: Compute, commonSize: Int, inpSize: Int, wgtSize: Int) extends PeekPokeTester(c) {

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

  val bitSize = 2 // data bitwidth, just for smaller numbers to print

  //Fill with data
  val r = new Random
  val inpGen = new RandomArray(commonSize, bitSize, r)
  val wgtGen = new RandomArray(commonSize, bitSize, r)
  val tnzrA = Array.fill(inpSize) { wgtGen.positive }
  val tnzrB = Array.fill(wgtSize) { wgtGen.positive }
  val tnzrAcc  = Array.fill(inpSize * wgtSize) {r.nextInt(pow(2, bitSize-1).toInt)}
  
  val memManager = new GeneralMemRead (tester = this)
  
  //println("-D- ACC ARRAY:" + tnzrAcc.mkString(" "))
  runCompute()
  
  
  val mp = c.mp // memory parameters

  def runCompute () = {
    loadAcc(memory = tnzrAcc, offset = 0, height = inpSize, width = wgtSize)
    
  }
  def loadAcc(memory: Array[Int], offset: Int, height: Int, width: Int) = {
    val accReadData = new  DRAMReadStateData (name = "acc",   /* memory name for printing */ 
                                          validDataSignal = c.io.vme_rd(1).data.valid, /* chisel name to poke data valid */
                                          readyDataSignal = c.io.vme_rd(1).data.ready, /* chisel name to poke data valid */
                                          dataSignal = c.io.vme_rd(1).data.bits.data, /* chisel name to poke data */
                                          dataTagSignal = c.io.vme_rd(1).data.bits.tag, /* chisel name to poke data */
                                          validIdxSignal = c.io.vme_rd(1).cmd.valid, /* chisel name to peek index valid */
                                          readyIdxSignal = c.io.vme_rd(1).cmd.ready, /* chisel name to poke data valid */
                                          idxSignal = c.io.vme_rd(1).cmd.bits.addr, /* chisel name to peek index*/
                                          lenSignal = c.io.vme_rd(1).cmd.bits.len, /* chisel name to peek index*/
                                          tagSignal = c.io.vme_rd(1).cmd.bits.tag, /* chisel name to peek index*/
                                          latency = () => 7 /* chisel name to peek index*/
                    ) {}
    val instr =  memManager.memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = 0,
                   xsize = 2,
                   ysize = 2,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = 0, //uop_baddr | (maskOffset & (dec.dram_offset << log2Ceil(base*size)))
                   sram_offset = 24, // scratchpad idx
                   id = memManager.memId("acc"), // it is private in ISA 
                   push_next = 0,
                   push_prev = 0,
                   pop_next = 0,
                   pop_prev = 0,
                   op = memManager.taskId("load") // it is private in ISA
                   )
    val accInst = c.io.inst
    poke(accInst.bits, instr)
    poke(accInst.valid, 1)
    step(1)
    poke(accInst.valid, 0)
    for (clk <- 0 until 100) {
      step(1)
      println(s"Step $clk")
      memManager.readDram(tnzrAcc, accReadData) 
    }
  }
}
class ComputeTest32x32x32 extends GenericTest( "Compute", (p:Parameters) => new Compute()(p), (c:Compute) => new TestCompute(c, 1024, 1024, 1024))
