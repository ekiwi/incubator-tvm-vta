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

package unittest.util

import java.io._

import scala.math._
import scala.collection.mutable.HashMap

import scala.io.Source
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import vta.core._
import vta.shell._
import vta.util.config._
import ISA._


// dram memory read request state data
class DRAMReadStateData (val validIndices: scala.collection.mutable.ListBuffer[(Int,Int,Int,BigInt,Boolean)] = new scala.collection.mutable.ListBuffer[(Int,Int,Int,BigInt,Boolean)], /* read requests (wait,index,len,tag,last) */ 
                     var pushData: Boolean = true, /* push a new data and set to valid */
                     var reads: Int = 0,  /* initial value of total reads number */
                     val name: String = "mem",   /* memory name for printing */ 
                     val validDataSignal: Bits, /* chisel name to poke data valid */
                     val readyDataSignal: Bits, /* chisel name to peek ready data */
                     val dataSignal: Bits, /* chisel name to poke data */
                     val dataTagSignal: Bits, /* chisel name to poke data dest*/
                     val dataLstSignal: Bits = Bits(0.W), /* chisel name to poke ack index */
                     val validIdxSignal: Bits, /* chisel name to peek index valid */
                     val readyIdxSignal: Bits, /* chisel name to poke ready index */
                     val idxSignal: Bits, /* chisel name to peek index*/
                     val lenSignal: Bits, /* chisel name to peek len */
                     val tagSignal: Bits, /* chisel name to peek tag */
                     val latency: () => Int /* function to return read request latency*/
                    ) {}
class DRAMWriteStateData (val validIndices: scala.collection.mutable.ListBuffer[(Int,Int,Boolean)] = new scala.collection.mutable.ListBuffer[(Int,Int,Boolean)], /* read requests (wait,index,ack) */ 
                     var pushData: Boolean = true, /* push a new data and set to valid */
                     var sendAck: Int = 0,  /* 0 - dont send,  1 - send, > 1 wait */
                     var reads: Int = 0,  /* initial value of total reads number */
                     val name: String = "mem",   /* memory name for printing */ 
                     val validDataSignal: Bits, /* chisel name to poke data valid */
                     val readyDataSignal: Bits, /* chisel name to peek ready data */
                     val dataSignal: Bits, /* chisel name to peek data */
                     val strbSignal: Bits, /* chisel name to peek data */
                     val validCmdSignal: Bits, /* chisel name to peek index valid */
                     val readyCmdSignal: Bits, /* chisel name to poke ready index */
                     val dataCmdSignal: Bits, /* chisel name to peek index*/
                     val lenSignal: Bits, /* chisel name to peek len */
                     val ackSignal: Bits =  Bits(0.W), /* chisel name to poke ack index */
                     val latency: () => Int /* function to return read request latency*/
                    ) {}
class VTAWriteStateData (
                     var sentCmds: Int = 0,  /* initial value of total reads number */
                     var sentDats: Int = 0,  /* initial value of total reads number */
                     val validDataSignal: Bits, /* chisel name to poke data valid */
                     val readyDataSignal: Bits, /* chisel name to peek ready data */
                     val dataSignal: Bits, /* chisel name to poke data */
                     val validCmdSignal: Bits, /* chisel name to peek index valid */
                     val readyCmdSignal: Bits, /* chisel name to poke ready index */
                     val dataCmdSignal: Bits, /* chisel name to peek index*/
                     val latency: () => Int /* function to return read request latency*/
                    ) {}
class VTACommStateData (
                     val validDataSignal: Bits, /* chisel name to poke data valid */
                     val readyDataSignal: Bits, /* chisel name to peek ready data */
                     val dataSignal: Bits, /* chisel name to poke data */
                     val validCmdSignal: Bits, /* chisel name to peek index valid */
                     val readyCmdSignal: Bits, /* chisel name to poke ready index */
                     val dataCmdSignal: Bits, /* chisel name to peek index*/
                     val latency: () => Int /* function to return read request latency*/
                    ) {}
class SRAMReadStateData (var valid: Boolean = false, /* initial value of data valid */ 
                    var index: Int = 0,     /* initial vlaue of tensor data index*/
                    var lastIndex: Int = -1, /* The previous value of index, used to track same address reads*/ 
                    var reads: Int = 0, /*  reads counter */
                    val tensorOuterSize: Int,  /* tensor outer size */
                    val tensorInnerSize: Int,  /* tensor inner size */
                    val alwaysRead: Boolean,  /* flags cout all read requests */
                    val name: String,    /* tensor name, used in pritouts */
                    val idxBegin: () => Int, /* function to get scratchpad index offset of tensor data*/
                    val validDataSig: Bits,  /* chisel signal name for tensor data valid */
                    val validIdxSig: Bits, /* chisel signal name for tensor index valid */
                    val dataSig: Vec[Vec[UInt]],  /* chisel signal name for tensor data*/
                    val idxSig: Bits /* chisel signal name for tensor index*/
                    ) {}
class TestUOP(val accRef:Int, val inpRef:Int, val wgtRef:Int) {
}
class UOPReadStateData (var valid: Boolean = false, /* initial value of data valid */ 
                    var index: Int = 0,     /* initial value of index*/
                    var lastIndex: Int = -1, /* The previous value of index, used to track same address reads*/ 
                    var reads: Int = 0, /*  reads counter */
                    val alwaysRead: Boolean,  /* flags cout all read requests */
                    val idxBegin: () => Int, /* function to get scratchpad index offset of tensor data*/
                    val validIdxSig: Bits,  /* chisel signal name for index valid */
                    val dataIdxSig: Bits, /* chisel signal name for index */
                    val dataU0Sig: Bits, /* chisel signal name for uop u0 field */
                    val dataU1Sig: Bits, /* chisel signal name for uop u1 field */
                    val dataU2Sig: Bits, /* chisel signal name for uop u2 field */
                    val dataValidSig: Bits /* chisel signal name for uop u fields valid */
                    ) {}

class GeneralMemRead [T <: Module] (tester: PeekPokeTester[T], dumpMemoryFileName: String = "")(implicit val p: Parameters) {
    
  
  val vp = p(ShellKey).vcrParams
  val mp = p(ShellKey).memParams
  //--------------------------------------------------------------------
  // memory dump functionality allows storing and loading memory state
  //--------------------------------------------------------------------
 
  val dumpMemory = dumpMemoryFileName != "" // dump memory transactions into dumpMemoryFileName
  val dumpWrites = true // dump memory writes
  val loadInstructions = false // load instructions form instructions dump file (dtill can be in the RawMemory)
  private var dumpMemSize = 0 // updated by read/writes
  private var memStr =  new scala.collection.mutable.ListBuffer[String] // updated by read/writes
  private val memInstrStr = new scala.collection.mutable.ListBuffer[String] // updated by read/writes
  private var memInstrStart = 0 // updated explicitly
  private var memInstrNb = 0 // updated explicitly
  private var assertWrites = false // assert destination is the same as source data.Canbe used if a memory dump is loaded
   
  val accMemBits = log2Ceil(p(CoreKey).accMemDepth)
  val inpMemBits = log2Ceil(p(CoreKey).inpMemDepth)
  val wgtMemBits = log2Ceil(p(CoreKey).wgtMemDepth)

  val inpFactorBits = p(CoreKey).inpFactorBits
  val wgtFactorBits = p(CoreKey).wgtFactorBits
  val accFactorBits = p(CoreKey).accFactorBits

  //-----------------//
  //--- Read DRAM ---//
  //-----------------//
  
  // a single memory transaction data size bytes
  val readBlockBytes = mp.dataBits/8

  //emulate VMEMaster response
  def readDram (memory: Array[Int], readState: DRAMReadStateData) = {

    val validDataSignal = readState.validDataSignal
    val readyDataSignal = readState.readyDataSignal
    val bitsSignal      = readState.dataSignal
    val bitsTagSignal   = readState.dataTagSignal
    val bitsLstSignal   = readState.dataLstSignal
    val validIdxSignal  = readState.validIdxSignal
    val readyIdxSignal  = readState.readyIdxSignal
    val cmdIdxSignal    = readState.idxSignal
    val cmdLenSignal    = readState.lenSignal
    val cmdTagSignal    = readState.tagSignal
    
    
    // one request
    var readyToReadCmd = true
    if (readState.validIndices.length > 256) { // changing from 0 to 256 to support buffering requests
      readyToReadCmd = false
      tester.poke( readyIdxSignal, 0)
    } else {
      tester.poke( readyIdxSignal, 1)
    }

    // load the first index data that is loaded
    // get ready to receive
    val clientReadyToReceiveData = tester.peek(readyDataSignal).toInt > 0
    if (clientReadyToReceiveData) readState.pushData = true
    
    var doRead = readState.pushData /* no data yet read*/
    var idx = 0
    //println(s"-D- Mem Read debug  clientReadyToReceiveData=$clientReadyToReceiveData readState.pushData=${readState.pushData} requestrs to send=${readState.validIndices.length}")
    
    if(bitsLstSignal.getWidth > 0) {
      tester.poke(bitsLstSignal, 0)
    }
    while (idx < readState.validIndices.length) {
      val (wait, memIdx, lenData, tagData, last) =  readState.validIndices(idx)
      require(wait >= 0)
      if (wait == 0 && doRead) {
        // expect memory idx in bytes
        val readSize = readBlockBytes
        require(memIdx <= memory.length - readSize, s"-F- Read out of memory ${readState.name} ${memIdx} ${memory.length}-$readSize ")
        var memData = BigInt(0)
        val dataList = new scala.collection.mutable.ListBuffer[String]
        // read readSize bytes
        for(idx <- 0 until readSize) {
          memData += BigInt(memory(memIdx+ idx))<< idx * 8
          dataList.append(BigInt(memory(memIdx+ idx)).toString(16))
        }
        tester.poke( bitsSignal, memData)
        tester.poke( bitsTagSignal, tagData)
        if(bitsLstSignal.getWidth > 0) {
          if (last) {
            tester.poke(bitsLstSignal, 1)
          } else {
            tester.poke(bitsLstSignal, 0)
          }
        }
        tester.poke( validDataSignal, 1)
        readState.validIndices.remove(idx)
        doRead = false // stop reading
        if(!clientReadyToReceiveData) readState.pushData = false 
					  //println(s"-D- Mem Read response name=${readState.name} mem idx=$memIdx last=$last bytes=${readSize} data=${memData.toString(16)}::${dataList.reverse.mkString(":")}")
      } else {
        // decrement wait counter
        //require(wait > 0, "-F- Not expected multiple data valid events. TODO: expand behavior to handle request completion")
        readState.validIndices(idx) = (if (wait == 0) 0 else wait - 1, memIdx, lenData, tagData, last)
        idx += 1
      }
    }
    // nothing was read
    if (doRead) {
      //println(s"-D- Mem Read data not valid")
      readState.pushData = true 
      tester.poke( validDataSignal, 0)
    }
    
    // read new index to load
    if (tester.peek(validIdxSignal).toInt > 0 && readyToReadCmd) {
      val delay = readState.latency()
      val idxData = tester.peek(cmdIdxSignal).toInt
      val lenData = tester.peek(cmdLenSignal).toInt
      val tagData = tester.peek(cmdTagSignal)
      for(idx <- 0 to lenData) {
        readState.validIndices.append((delay, idxData + (idx * readBlockBytes), lenData, tagData, if(idx == lenData) true else false))
      }
      //println(s"-D- Mem Read request name=${readState.name} mem idx=$idxData len=$lenData tag=${tagData.toString(16)} delay=$delay")
      if (dumpMemory) {
        val prefix = if (memStr.length == 0) """  "RawMemory" :[""" else ","
        memStr.append(prefix + "\n" + s"""{ "Offset" : ${idxData} , "Data" : [${memory.slice(idxData, idxData + (lenData+1)*readBlockBytes).mkString(",")}]}  """)
      }
      val writeEnd = idxData + (lenData + 1)*readBlockBytes
      if (writeEnd > dumpMemSize) dumpMemSize = writeEnd
    }
  }

  //emulate VMEMaster response
  def writeDram (memory: Array[Int], writeState: DRAMWriteStateData) = {

    val validDataSignal = writeState.validDataSignal
    val readyDataSignal = writeState.readyDataSignal
    val dataSignal      = writeState.dataSignal
    val strbSignal      = writeState.strbSignal
    val validCmdSignal  = writeState.validCmdSignal
    val readyCmdSignal  = writeState.readyCmdSignal
    val dataCmdSignal   = writeState.dataCmdSignal
    val bitsLenSignal   = writeState.lenSignal
    val ackSignal       = writeState.ackSignal
    
    
    // send ack
    if(ackSignal.getWidth > 0) {
      if (writeState.sendAck == 1) {
        tester.poke(ackSignal, 1)
      } else {
        tester.poke(ackSignal, 0)
      }
      if (writeState.sendAck > 0) {
        writeState.sendAck -= 1
      }
    }
    
    // allow one request
    var readyForCmd = true
    if (writeState.validIndices.length > 0) {
      readyForCmd = false
      tester.poke( readyCmdSignal, 0)
    } else {
      tester.poke( readyCmdSignal, 1)
    }
    
    if (readyForCmd && tester.peek(validCmdSignal).toInt > 0) {
      val delay = writeState.latency()
      val idxData = tester.peek(dataCmdSignal).toInt
      val lenData = tester.peek(bitsLenSignal).toInt
      for(idx <- 0 to lenData) {
        writeState.validIndices.append((delay, idxData + (idx * readBlockBytes), /* ack signal with the last transfer*/if(idx == lenData) true else false))
      }
      //println(s"-D- Mem Write request name=${writeState.name} mem idx=$idxData len=$lenData delay=$delay")
      
    }
    
    var doRead = tester.peek(validDataSignal) > 0
    var idx = 0
    while (idx < writeState.validIndices.length) {
      val (wait, memIdx, ack) =  writeState.validIndices(idx)
      require(wait >= 0)
      if (wait == 0 && doRead) {
        // expect memory idx in bytes
        val memAddr = (memIdx & ~((BigInt(1) << log2Ceil(readBlockBytes)) - 1)).toInt
        val readSize = readBlockBytes
        tester.poke(readyDataSignal, 1)
        val memStrb = tester.peek(strbSignal) // read block valid bytes
        var memData = tester.peek(dataSignal) // read one block of data
        var lastValidByte = 0
        var firstValidByte = readSize
        for (idx1 <- 0 until readSize) {
          if (((memStrb >> idx1) & 1) == 1) {
            lastValidByte = idx1
            if (firstValidByte > idx1) {
              firstValidByte = idx1
            }
          }
        }
        require(memAddr <= memory.length - lastValidByte, s"-F- Write out of memory ${writeState.name} ${memAddr} ${memory.length}-$lastValidByte ")
        // read readSize bytes
        val byteMask = (BigInt(1) << 8) - 1
        // read bytes
        //println(s"-D- Mem Write data peek idx=$memIdx addr=$memAddr data=${memData.toString(16)} strb=${memStrb.toString(16)}")
        for(idx1 <- 0 until readSize) {
          val writeData = memData & byteMask
          memData = memData >> 8
          //println(s"-D- Mem memStrb bit $idx1 ${((memStrb >> idx1) & 1)} data = $writeData")
          if (((memStrb >> idx1) & 1) == 1) {
            if (assertWrites) {
              if (memory(memAddr + idx1) != writeData.toInt) {
                end()
                require(false, s"-F- Invalid write data idx=${memAddr + idx1} ${memory(memAddr + idx1)}!=${writeData.toInt}")
              }
            } else {
              memory(memAddr + idx1) = writeData.toInt
            }
          }
        }
        if (dumpMemory && dumpWrites) {
          val prefix = if (memStr.length == 0) """  ,"RawMemory" :[""" else ","
          memStr.append(prefix + "\n" + s"""{ "Offset" : ${memIdx} , "Data" : [${memory.slice(memIdx, memIdx + lastValidByte - firstValidByte).mkString(",")}]}  """)
        }
        val writeEnd = memIdx + lastValidByte - firstValidByte
        if (writeEnd > dumpMemSize) dumpMemSize = writeEnd
        writeState.validIndices.remove(idx)
        doRead = false // stop reading
        if(ack) {
          writeState.sendAck = writeState.latency()
        }
        //println(s"-D- Mem Write response idx=$memIdx data=${memData.toString(2)}")
      } else {
        // decrement wait counter
        //require(wait > 0, "-F- Not expected multiple data valid events. TODO: expand behavior to handle request completion")
        writeState.validIndices(idx) = (if (wait == 0) 0 else wait - 1, memIdx, ack)
      }
      idx += 1
    }
   
    if (doRead /* read happened */) {
      tester.poke(readyDataSignal, 0)
    }
  }

  def writeVTA (memory: Array[Int], cmdBeginDram:Int, cmdNb:Int, cmdSizeByte:Int, writeState: VTAWriteStateData) = {

    val validDataSignal = writeState.validDataSignal
    val readyDataSignal = writeState.readyDataSignal
    val dataSignal      = writeState.dataSignal
    val validCmdSignal  = writeState.validCmdSignal
    val readyCmdSignal  = writeState.readyCmdSignal
    val dataCmdSignal   = writeState.dataCmdSignal
    
    
    // first set all regs except the 0th one
    // The 0th reg is for launch
    val readyForCmd = tester.peek(readyCmdSignal).toInt > 0
    if (readyForCmd && writeState.sentCmds < cmdNb - 1) {
      tester.poke( validCmdSignal, 1)
      tester.poke( dataCmdSignal, (writeState.sentCmds + 1)*cmdSizeByte) // the address is from 1 to a number of cmds
      writeState.sentCmds += 1
    } else if (readyForCmd && writeState.sentCmds == cmdNb - 1) {
      tester.poke( validCmdSignal, 1)
      tester.poke( dataCmdSignal, 0) // send 0th reg
      writeState.sentCmds += 1
    } else {
       tester.poke( validCmdSignal, 0)
    }
    
    val readyForData = tester.peek(readyDataSignal).toInt > 0
    if (readyForData && writeState.sentDats < cmdNb - 1) {
      val start = cmdBeginDram + (writeState.sentDats + 1) * cmdSizeByte
      val end   = start + cmdSizeByte
      val cmdData = constructData(memory.slice(start, end).map(data => (data, 8)))._1
      tester.poke( validDataSignal, 1)
      tester.poke( dataSignal, cmdData)
      writeState.sentDats += 1
    } else if (readyForData && writeState.sentDats == cmdNb - 1) {
      val start = cmdBeginDram
      val end   = start + cmdSizeByte
      val cmdData = constructData(memory.slice(start, end).map(data => (data, 8)))._1
      tester.poke( validDataSignal, 1)
      tester.poke( dataSignal, cmdData)
      writeState.sentDats += 1
    } else {
       tester.poke( validDataSignal, 0)
    }
    
  }
  def checkHost (regIdx : Int, commState: VTACommStateData) = {

    val validDataSignal = commState.validDataSignal
    val readyDataSignal = commState.readyDataSignal
    val dataSignal      = commState.dataSignal
    val validCmdSignal  = commState.validCmdSignal
    val readyCmdSignal  = commState.readyCmdSignal
    val dataCmdSignal   = commState.dataCmdSignal
    
    tester.poke( validCmdSignal, 1)
    tester.poke( dataCmdSignal, regIdx) // read reg
    tester.poke( readyDataSignal, 1) // read data

    val dataValid = tester.peek(validDataSignal).toInt > 0
    val dataValue = tester.peek(dataSignal)
    if (dataValid) {
      dataValue.toInt
    } else -1
  }


  // read scratchpad sram
  // respond to TensorMasterData
  // set tensor from matrix
  def readSram(matrix: Array[Array[Int]], dataState: SRAMReadStateData) = {

    val outerSize = matrix.length
    require(outerSize > 0)
    val innerSize = matrix(0).length
    require(innerSize > 0)

    val validDataSignal = dataState.validDataSig
    val validIdxSignal  = dataState.validIdxSig
    val bitsIdxSignal   = dataState.idxSig
    if (dataState.valid) {
      val outerTSize = ceil(outerSize / dataState.tensorOuterSize.toFloat).toInt
      val innerTSize = ceil(innerSize / dataState.tensorInnerSize.toFloat).toInt
      val outerIdx = (dataState.index / innerTSize) * dataState.tensorOuterSize
      val innerIdx = (dataState.index % innerTSize) * dataState.tensorInnerSize
      //println(s"-D- read ${dataState.name}: read idx=${dataState.index} to outerIdx=$outerIdx innerIdx=$innerIdx")
      for (j <- 0 until dataState.tensorOuterSize) {
        for (k <- 0 until dataState.tensorInnerSize) {
          if (dataState.index != dataState.lastIndex || dataState.alwaysRead) {
            dataState.reads += 1
          }
          val bitsSignal = dataState.dataSig(j)(k)
          if (outerIdx + j >= outerSize || innerIdx + k >= innerSize) {
            tester.poke( bitsSignal, 0)
          } else {
            tester.poke( bitsSignal, matrix(outerIdx + j)(innerIdx + k))
          }
        }
      }
      dataState.lastIndex = dataState.index
      tester.poke( validDataSignal, 1)
    } else {
      tester.poke( validDataSignal, 0)
    }
    dataState.valid = tester.peek(validIdxSignal).toInt > 0
    
    dataState.index = if (dataState.valid) {
      val nval = tester.peek(bitsIdxSignal).toInt - dataState.idxBegin()
      nval
    } else {
      0
    }
  }

  // read tensor and store it into array
  def writeSram(tnzrAcc: Array[Array[Int]], 
                batch: Int,
                size: Int,
                idxOffset: Int,
                validSignal: Bits, 
                dataSignal: Vec[Vec[UInt]], 
                indexSignal: Bits) = {

    val outerSize = tnzrAcc.length
    require(outerSize > 0)
    val innerSize = tnzrAcc(0).length
    require(innerSize > 0)
    
    var accWrites = 0
    
    val accWrValid = tester.peek(validSignal).toInt > 0
    val accWrIdx = tester.peek(indexSignal).toInt -  idxOffset
    if (accWrValid) {
      val outerTSize = ceil(outerSize / batch).toInt
      val innerTSize = ceil(innerSize / size.toFloat).toInt
      val outerIdx = (accWrIdx / innerTSize) * batch
      val innerIdx = (accWrIdx % innerTSize) * size
      //println(s"-D- writeAcc: write idx=$accWrIdx to outerIdx=$outerIdx innerIdx=$innerIdx")
      for (j <- 0 until batch) {
        for (k <- 0 until size) {
          accWrites += 1
          val accData =  tester.peek(dataSignal(j)(k)).toInt
          if (outerIdx + j >= outerSize || innerIdx + k >= innerSize) {
            require(accData == 0, s"-F- Padded area must not accumulate values outer=${outerIdx + j} inner=${innerIdx + k} $accData")
          } else {
            tnzrAcc(outerIdx + j)(innerIdx + k) = accData
          }
        }
      }
    }
    accWrites
  }
  
  def readUOP(uops: Array[TestUOP],  dataState: UOPReadStateData) = {
    if (dataState.valid) {
      tester.poke( dataState.dataU0Sig, uops(dataState.index).accRef)
      tester.poke( dataState.dataU1Sig, uops(dataState.index).inpRef)
      tester.poke( dataState.dataU2Sig, uops(dataState.index).wgtRef)
      tester.poke( dataState.dataValidSig, 1)
      //println(s"-D- readUOP: read idx=${dataState.index} accRef=${uops(dataState.index).accRef} inpRef=${uops(dataState.index).inpRef} wgtRef=${uops(dataState.index).wgtRef} ")
    } else {
      tester.poke( dataState.dataValidSig, 0)
    }
    
    dataState.valid = tester.peek(dataState.validIdxSig).toInt > 0

    if (dataState.valid) {
      dataState.reads += 1
      dataState.index = tester.peek(dataState.dataIdxSig).toInt - dataState.idxBegin()
    }
  }

  //------------------------------//
  //--- Construct instructions ---//
  //------------------------------//
  // From private ISA.scala
  val taskId: HashMap[String, String] =
    HashMap(("load", "000"),
      ("store", "001"),
      ("gemm", "010"),
      ("finish", "011"),
      ("alu", "100"))

  val memId: HashMap[String, String] =
    HashMap(("uop", "00"), ("wgt", "01"), ("inp", "10"), ("acc", "11"))

  val aluId: HashMap[String, String] =
    HashMap(("minpool", "00"),
      ("maxpool", "01"),
      ("add", "10"),
      ("shift", "11"))
  
  def fieldToData(field: BigInt, mask:Int, offset: Int) = {
    val data = (field & ((BigInt(1) << mask) - 1))
    require(data == field, s"-F- Invalid inst field data range field=${field} width=${mask}")
    data << offset
  }
  def constructData (data: Seq[(Int, Int)]) = {
    data.foldLeft((BigInt(0), 0)) {case (acc, item) => {
                   val data = fieldToData(item._1, item._2, acc._2)
                   (data + acc._1, item._2 + acc._2)
                 }
               }
  }
  //src/main/scala/core/Decode.scala
  def memInst(xpad_1: Int,      
                   xpad_0: Int,      
                   ypad_1: Int,      
                   ypad_0: Int,      
                   xstride: Int,     
                   xsize: Int,       
                   ysize: Int,       
                   empty_0: Int,
                   is_min_pad_value: Int,
                   dram_offset: Int, 
                   sram_offset: Int, 
                   id: String,          
                   push_next: Int,   
                   push_prev: Int,   
                   pop_next: Int,    
                   pop_prev: Int,    
                   op: String          
                   ) : BigInt = {
    
    val inst = List(
     (Integer.parseInt(op,2), OP_BITS),
     (pop_prev, 1),
     (pop_next, 1),
     (push_prev, 1),
     (push_next, 1),
     (Integer.parseInt(id,2), M_ID_BITS),
     (sram_offset, M_SRAM_OFFSET_BITS),
     (dram_offset, M_DRAM_OFFSET_BITS),
     (is_min_pad_value, 1),
     (empty_0, 6),
     (ysize, M_SIZE_BITS),
     (xsize, M_SIZE_BITS),
     (xstride, M_STRIDE_BITS),
     (ypad_0, M_PAD_BITS),
     (ypad_1, M_PAD_BITS),
     (xpad_0, M_PAD_BITS),
     (xpad_1, M_PAD_BITS)
    )
    
    val data =  constructData(inst)._1

    //println(s"-D- Create MEM INST: op=${op} id=${id} ${data.toString(2)}")
    val instrType = if (op == taskId("store")) {
        if (xsize == 0) "StorSnc" else "Stor"
      } else if (op == taskId("load")) {
        if (id == memId("inp")) {
          if (xsize == 0) "LoadSnc" else "LoadInp"
        } else if (id == memId("wgt")) {
          if (xsize == 0) "LoadSnc" else "LoadWgt"
        }  else if (id == memId("uop")) {
          if (xsize == 0) "CompSnc" else "CompUop"
        }  else if (id == memId("acc")) {
          if (xsize == 0) "CompSnc" else "CompAcc"
        } else {
          require(false, "-F- Unsupported id=${id}")
        }
      } else {
          require(false, "-F- Unsupported op=${op}")
      }
    if (dumpMemory) {
      val prefix = if (memInstrStr.length == 0) {
        """  ,"Instructions" :[""" +
        "\n  {" +
        """ "Offset" : -1,""" +
        """ "Data" : [ """
      } else ","
      val instTxt = prefix + "\n" + s"""{"TYPE":"${instrType}",""" +
           s""" "push_next":${push_next},""" +
           s""" "push_prev":${push_prev},""" +
           s""" "pop_next":${pop_next},""" +
           s""" "pop_prev":${pop_prev},""" +
           s""" "xpad_1":${xpad_1},""" +
           s""" "xpad_0":${xpad_0},""" +
           s""" "ypad_1":${ypad_1},""" +
           s""" "ypad_0":${ypad_0},""" +
           s""" "xstride":${xstride},""" +
           s""" "xsize":${xsize},""" +
           s""" "ysize":${ysize},""" +
           s""" "is_min_pad_value":${is_min_pad_value},""" +
           s""" "dram_offset":${dram_offset},""" +
           s""" "sram_offset":${sram_offset},""" +
           s""" "id":"${id}",""" +
           s""" "op":"${op}" """ +
           """}"""
      memInstrStr.append(instTxt)
      //println(s"-D- INSTR: ${data.toString(16)} ${instTxt}")
    }
    data
  }

  def syncCompInst(    push_next: Int,   
                   push_prev: Int,   
                   pop_next: Int,    
                   pop_prev: Int          
                   ) = {
    
    // sync is zero size comp load
    memInst(       xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = 0,
                   xsize = 0, // for sync
                   ysize = 0,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = 0,
                   sram_offset = 0,
                   id = memId("uop"), //comp
                   push_next = push_next,
                   push_prev = push_prev,
                   pop_next = pop_next,
                   pop_prev = pop_prev,
                   op = taskId("load") // load
                   )
  }
  def syncStoreInst(    push_next: Int,   
                   push_prev: Int,   
                   pop_next: Int,    
                   pop_prev: Int          
                   ) = {
    
    // sync is zero size store
    memInst(
                   xpad_1 = 0,
                   xpad_0 = 0,
                   ypad_1 = 0,
                   ypad_0 = 0,
                   xstride = 0,
                   xsize = 0, // isSync
                   ysize = 0,
                   empty_0 = 0,
                   is_min_pad_value = 0,
                   dram_offset = 0, 
                   sram_offset = 0, 
                   id = memId("uop"), // not used
                   push_next = push_next,
                   push_prev = push_prev, 
                   pop_next = pop_next,
                   pop_prev = pop_prev, 
                   op = taskId("store") // store
                   )  }
  def finInst(push_next: Int,   
               push_prev: Int,   
               pop_next: Int,    
               pop_prev: Int,    
               op: String) : BigInt = {
    
    val inst = List(
     (Integer.parseInt(op,2), OP_BITS),
     (pop_prev, 1),
     (pop_next, 1),
     (push_prev, 1),
     (push_next, 1),
     (0, INST_BITS - OP_BITS - 4)
    )
    
    val data =  constructData(inst)._1
    if (dumpMemory) {
      val prefix = if (memInstrStr.length == 0) {
        """  ,"Instructions" :[""" +
        "\n  {" +
        """ "Offset" : -1,""" +
        """ "Data" : [ """
      } else ","
      val instTxt = prefix + "\n" + s"""{"TYPE":"CompFin",""" +
             s""" "push_next":${push_next},""" +
             s""" "push_prev":${push_prev},""" +
             s""" "pop_next":${pop_next},""" +
             s""" "pop_prev":${pop_prev},""" +
             s""" "op":"${op}" """ +
             """}"""
      memInstrStr.append(instTxt)
      //println(s"-D- INSTR: ${data.toString(16)} ${instTxt}")
    }  

    //println(s"-D- Create MEM INST: ${data.toString(2)}")
    data
  }
  def gemInst(empty_1: Int,     
              wgt_1: Int,      
               wgt_0: Int,      
               inp_1: Int,      
               inp_0: Int,      
               acc_1: Int,     
               acc_0: Int,       
               empty_0: Int,     
               lp_1: Int, 
               lp_0: Int, 
               uop_end: Int, 
               uop_begin: Int, 
               reset: Int,          
               push_next: Int,   
               push_prev: Int,   
               pop_next: Int,    
               pop_prev: Int,    
               op: String          
               ) : BigInt = {
    
    val inst = List(
     (Integer.parseInt(op,2), OP_BITS),
     (pop_prev, 1),
     (pop_next, 1),
     (push_prev, 1),
     (push_next, 1),
     (reset, 1),
     (uop_begin, log2Ceil(p(CoreKey).uopMemDepth)),
     (uop_end, log2Ceil(p(CoreKey).uopMemDepth) + 1),
     (lp_0, C_ITER_BITS),
     (lp_1, C_ITER_BITS),
     (empty_0, INST_BITS/2 - OP_BITS - 5 - (2 * log2Ceil(p(CoreKey).uopMemDepth)) - 1 - (2 * C_ITER_BITS)),
     (acc_0, accFactorBits),
     (acc_1, accFactorBits),
     (inp_0, inpFactorBits),
     (inp_1, inpFactorBits),
     (wgt_0, wgtFactorBits),
     (wgt_1, wgtFactorBits),
     (empty_1, INST_BITS/2 - 2*p(CoreKey).wgtFactorBits - 2*p(CoreKey).inpFactorBits - 2*p(CoreKey).accFactorBits)
    )
    val data =  constructData(inst)._1

    //println(s"-D- Create GEMM INST: ${data.toString(2)}")
    val instrType = if(reset > 0) "CompGemReset" else "CompGem"
    
    if (dumpMemory) {
      val prefix = if (memInstrStr.length == 0) {
        """  ,"Instructions" :[""" +
        "\n  {" +
        """ "Offset" : -1,""" +
        """ "Data" : [ """
      } else ","
      val instTxt = prefix + "\n" + s"""{"TYPE":"${instrType}",""" +
             s""" "push_next":${push_next},""" +
             s""" "push_prev":${push_prev},""" +
             s""" "pop_next":${pop_next},""" +
             s""" "pop_prev":${pop_prev},""" +
             s""" "wgt_1":${wgt_1},""" +
             s""" "wgt_0":${wgt_0},""" +
             s""" "inp_1":${inp_1},""" +
             s""" "inp_0":${inp_0},""" +
             s""" "acc_1":${acc_1},""" +
             s""" "acc_0":${acc_0},""" +
             s""" "lp_1":${lp_1},""" +
             s""" "lp_0":${lp_0},""" +
             s""" "uop_end":${uop_end},""" +
             s""" "uop_begin":${uop_begin},""" +
             s""" "reset":${reset},""" +
             s""" "op":"${op}" """ +
             """}"""
      memInstrStr.append(instTxt)
      //println(s"-D- INSTR: ${data.toString(16)} ${instTxt}")
    }
    data
  }
  def gemInst(dataMap: Map[String, Any], rst: Boolean = false ) : BigInt = {
  
  
    val wgt_1 = if (dataMap contains "wgt_1") dataMap("wgt_1").asInstanceOf[Int] else 0      
    val wgt_0 = if (dataMap contains "wgt_0") dataMap("wgt_0").asInstanceOf[Int] else 0      
    val inp_1 = if (dataMap contains "inp_1") dataMap("inp_1").asInstanceOf[Int] else 0      
    val inp_0 = if (dataMap contains "inp_0") dataMap("inp_0").asInstanceOf[Int] else 0      
    val acc_1 = if (dataMap contains "acc_1") dataMap("acc_1").asInstanceOf[Int] else 0      
    val acc_0 = if (dataMap contains "acc_0") dataMap("acc_0").asInstanceOf[Int] else 0      
    val lp_1 = if (dataMap contains "lp_1") dataMap("lp_1").asInstanceOf[Int] else 0      
    val lp_0 = if (dataMap contains "lp_0") dataMap("lp_0").asInstanceOf[Int] else 0      
    val uop_end = if (dataMap contains "uop_end") dataMap("uop_end").asInstanceOf[Int] else 1      
    val uop_begin = if (dataMap contains "uop_begin") dataMap("uop_begin").asInstanceOf[Int] else 0      
    val reset = if (dataMap contains "reset") dataMap("reset").asInstanceOf[Int] else if (rst) 1 else 0
    val push_next = if (dataMap contains "push_next") dataMap("push_next").asInstanceOf[Int] else 0      
    val push_prev = if (dataMap contains "push_prev") dataMap("push_prev").asInstanceOf[Int] else 0      
    val pop_next = if (dataMap contains "pop_next") dataMap("pop_next").asInstanceOf[Int] else 0      
    val pop_prev = if (dataMap contains "pop_prev") dataMap("pop_prev").asInstanceOf[Int] else 0      
    val op = taskId("gemm")          
    
    gemInst( empty_1 = 0, wgt_1 = wgt_1, wgt_0 = wgt_0, inp_1 = inp_1, inp_0 = inp_0,      
             acc_1 = acc_1, acc_0 = acc_0, empty_0 = 0, lp_1 = lp_1, 
             lp_0 = lp_0, uop_end = uop_end, uop_begin = uop_begin, reset = reset,
             push_next = push_next, push_prev = push_prev,   
             pop_next = pop_next, pop_prev = pop_prev, op = op)
  }  
  def aluInst(dataMap: Map[String, Any]) : BigInt = {
 
    val alu_imm = if (dataMap contains "alu_imm") dataMap("alu_imm").asInstanceOf[Int] else 0      
    val alu_use_imm = if (dataMap contains "alu_use_imm") dataMap("alu_use_imm").asInstanceOf[Int] else 0      
    val alu_op = if (dataMap contains "alu_op") dataMap("alu_op").asInstanceOf[String] else aluId("add")      
    val src_1 = if (dataMap contains "src_1") dataMap("src_1").asInstanceOf[Int] else 0      
    val src_0 = if (dataMap contains "src_0") dataMap("src_0").asInstanceOf[Int] else 0      
    val dst_1 = if (dataMap contains "dst_1") dataMap("dst_1").asInstanceOf[Int] else 0      
    val dst_0 = if (dataMap contains "dst_0") dataMap("dst_0").asInstanceOf[Int] else 0      
    val lp_1 = if (dataMap contains "lp_1") dataMap("lp_1").asInstanceOf[Int] else 0      
    val lp_0 = if (dataMap contains "lp_0") dataMap("lp_0").asInstanceOf[Int] else 0      
    val uop_end = if (dataMap contains "uop_end") dataMap("uop_end").asInstanceOf[Int] else 1      
    val uop_begin = if (dataMap contains "uop_begin") dataMap("uop_begin").asInstanceOf[Int] else 0      
    val reset = if (dataMap contains "reset") dataMap("reset").asInstanceOf[Int] else 0
    val push_next = if (dataMap contains "push_next") dataMap("push_next").asInstanceOf[Int] else 0      
    val push_prev = if (dataMap contains "push_prev") dataMap("push_prev").asInstanceOf[Int] else 0      
    val pop_next = if (dataMap contains "pop_next") dataMap("pop_next").asInstanceOf[Int] else 0      
    val pop_prev = if (dataMap contains "pop_prev") dataMap("pop_prev").asInstanceOf[Int] else 0      
    val op = taskId("alu")          
    
    aluInst( empty_1 = 0, alu_imm = alu_imm, alu_use_imm = alu_use_imm, alu_op = alu_op, src_1 = src_1,      
             src_0 = src_0, dst_1 = dst_1, dst_0 = dst_0, empty_0 = 0, lp_1 = lp_1, 
             lp_0 = lp_0, uop_end = uop_end, uop_begin = uop_begin, reset = reset,
             push_next = push_next, push_prev = push_prev,   
             pop_next = pop_next, pop_prev = pop_prev, op = op)
  }  
  def memInst(dataMap: Map[String, Any], 
             instId: String = memId("uop"), 
             instOp: String = taskId("load"), 
             tsize: Int = 0) : BigInt = {
  
  
    val xpad_1 = if (dataMap contains "xpad_1") dataMap("xpad_1").asInstanceOf[Int] else 0      
    val xpad_0 = if (dataMap contains "xpad_0") dataMap("xpad_0").asInstanceOf[Int] else 0      
    val ypad_1 = if (dataMap contains "ypad_1") dataMap("ypad_1").asInstanceOf[Int] else 0      
    val ypad_0 = if (dataMap contains "ypad_0") dataMap("ypad_0").asInstanceOf[Int] else 0      
    val xstride = if (dataMap contains "xstride") dataMap("xstride").asInstanceOf[Int] else 0      
    val xsize = if (dataMap contains "xsize") dataMap("xsize").asInstanceOf[Int] else tsize      
    val ysize = if (dataMap contains "ysize") dataMap("ysize").asInstanceOf[Int] else 0
    val is_min_pad_value = if (dataMap contains "is_min_pad_value") dataMap("is_min_pad_value").asInstanceOf[Int] else 0
    val dram_offset = if (dataMap contains "dram_offset") dataMap("dram_offset").asInstanceOf[Int] else 0      
    val sram_offset = if (dataMap contains "sram_offset") dataMap("sram_offset").asInstanceOf[Int] else 1      
    val id = if (dataMap contains "id") dataMap("id").asInstanceOf[String] else instId      
    val push_next = if (dataMap contains "push_next") dataMap("push_next").asInstanceOf[Int] else 0      
    val push_prev = if (dataMap contains "push_prev") dataMap("push_prev").asInstanceOf[Int] else 0      
    val pop_next = if (dataMap contains "pop_next") dataMap("pop_next").asInstanceOf[Int] else 0      
    val pop_prev = if (dataMap contains "pop_prev") dataMap("pop_prev").asInstanceOf[Int] else 0      
    val op = if (dataMap contains "op") dataMap("op").asInstanceOf[String] else instOp          
    
    memInst(       xpad_1 = xpad_1,
                   xpad_0 = xpad_0,
                   ypad_1 = ypad_1,
                   ypad_0 = ypad_0,
                   xstride = xstride,
                   xsize = xsize, // 0 for sync
                   ysize = ysize,
                   empty_0 = 0,
                   is_min_pad_value = is_min_pad_value,
                   dram_offset = dram_offset,
                   sram_offset = sram_offset,
                   id = id,
                   push_next = push_next,
                   push_prev = push_prev,
                   pop_next = pop_next,
                   pop_prev = pop_prev,
                   op = op
                   )
  }  
  def finInst(dataMap: Map[String, Any]) : BigInt = {
  
  
    val push_next = if (dataMap contains "push_next") dataMap("push_next").asInstanceOf[Int] else 0      
    val push_prev = if (dataMap contains "push_prev") dataMap("push_prev").asInstanceOf[Int] else 0      
    val pop_next = if (dataMap contains "pop_next") dataMap("pop_next").asInstanceOf[Int] else 1 //it is a compute instr that  usually comes after final store      
    val pop_prev = if (dataMap contains "pop_prev") dataMap("pop_prev").asInstanceOf[Int] else 0      
    val op = if (dataMap contains "op") dataMap("op").asInstanceOf[String] else taskId("finish")          
    
    finInst( push_next = push_next,
             push_prev = push_prev,
             pop_next = pop_next,
             pop_prev = pop_prev,
             op = op
           )
  }  

  // create uop data
  def uop( accIdx: Int, inpIdx: Int, wgtIdx: Int, empty_0: Int = 0) = {
    
    // decoding see TensorGemm
    // ordering/bitsize see Decode
    //println(s"-D- Create uop wgt=${wgtIdx}:${log2Ceil(p(CoreKey).wgtMemDepth)} inp=${inpIdx}:${log2Ceil(p(CoreKey).inpMemDepth)} acc=${accIdx}:${log2Ceil(p(CoreKey).accMemDepth)}")
    val inst = List(
     (accIdx, accMemBits), //u0: log2Ceil(p(CoreKey).accMemDepth)
     (inpIdx, inpMemBits), //u1:log2Ceil(p(CoreKey).inpMemDepth)
     (empty_0, if ((accMemBits + inpMemBits + wgtMemBits > 32) && (wgtMemBits <= 32)) { 
       32 - accMemBits - inpMemBits } else { 0 }),
     (wgtIdx, wgtMemBits) //u2: log2Ceil(p(CoreKey).wgtMemDepth)
    )
    val data =  constructData(inst)._1

    println(s"-D- Create UOP: (u2:u1:u0) (wgt:inp:acc) ${wgtIdx}:${inpIdx}:${accIdx} -> ${data.toString(2)}")
    data
  }
  
  def aluInst( empty_1: Int,      
               alu_imm: Int,      
               alu_use_imm: Int,      
               alu_op: String,      
               src_1: Int,     
               src_0: Int,       
               dst_1: Int,     
               dst_0: Int, 
               empty_0: Int, 
               lp_1: Int, 
               lp_0: Int, 
               uop_end: Int,          
               uop_begin: Int,   
               reset: Int,          
               push_next: Int,   
               push_prev: Int,   
               pop_next: Int,    
               pop_prev: Int,    
               op: String          
               ) : BigInt = {
    
    val inst = List(
     (Integer.parseInt(op,2), OP_BITS),
     (pop_prev, 1),
     (pop_next, 1),
     (push_prev, 1),
     (push_next, 1),
     (reset, 1),
     (uop_begin, log2Ceil(p(CoreKey).uopMemDepth)),
     (uop_end, log2Ceil(p(CoreKey).uopMemDepth) + 1),
     (lp_0, C_ITER_BITS),
     (lp_1, C_ITER_BITS),
     (empty_0, INST_BITS/2 - OP_BITS - 5 - (2 * log2Ceil(p(CoreKey).uopMemDepth)) - 1 - (2 * C_ITER_BITS)),
     (dst_0, accFactorBits),
     (dst_1, accFactorBits),
     (src_0, inpFactorBits),
     (src_1, inpFactorBits),
     (Integer.parseInt(alu_op,2), C_ALU_DEC_BITS),
     (alu_use_imm, 1),
     (alu_imm, C_ALU_IMM_BITS),
     (empty_1, INST_BITS/2 - C_ALU_IMM_BITS - 1 - C_ALU_DEC_BITS - 2*p(CoreKey).inpFactorBits - 2*p(CoreKey).accFactorBits)
    )
    
    val data =  constructData(inst)._1

    //println(s"-D- Create ALU INST: ${data.toString(2)}")
    if (dumpMemory) {
      val prefix = if (memInstrStr.length == 0) {
        """  ,"Instructions" :[""" +
        "\n  {" +
        """ "Offset" : -1,""" +
        """ "Data" : [ """
      } else ","
      val instTxt = prefix + "\n" + s"""{"TYPE":"CompAlu",""" +
             s""" "push_next":${push_next},""" +
             s""" "push_prev":${push_prev},""" +
             s""" "pop_next":${pop_next},""" +
             s""" "pop_prev":${pop_prev},""" +
             s""" "alu_imm":${alu_imm},""" +
             s""" "alu_use_imm":${alu_use_imm},""" +
             s""" "alu_op":${alu_op},""" +
             s""" "src_1":${src_1},""" +
             s""" "src_0":${src_0},""" +
             s""" "dst_1":${dst_1},""" +
             s""" "dst_0":${dst_0},""" +
             s""" "lp_1":${lp_1},""" +
             s""" "lp_0":${lp_0},""" +
             s""" "uop_end":${uop_end},""" +
             s""" "uop_begin":${uop_begin},""" +
             s""" "reset":${reset},""" +
             s""" "op":"${op}" """ +
             """}"""
      memInstrStr.append(instTxt)
      //println(s"-D- INSTR: ${data.toString(16)} ${instTxt}")
    }
    data
  }
  
  // Copy from bigger array to a smaller one
  // row - source row
  // col - source col
  // width - read width (inner index) not used must correspond to dst with
  // height - read height (outer index) not used must correspond to dst height
  def readSubArray(src: Array[Array[Int]], dst: Array[Array[Int]], 
                   row: Int, col: Int, width: Int, height: Int) = {
    val outSrcSize = src.length
    require(outSrcSize > 0)
    val innSrcSize = src(0).length
    require(innSrcSize > 0)
    val outDstSize = dst.length
    require(outDstSize > 0)
    val innDstSize = dst(0).length
    require(innDstSize > 0)
    
    require(width  == innDstSize)
    require(height == outDstSize)
    
    //println(s"-D- readSubArray: read row=$row col=$col width=$width height=$height")
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        if (col + j >= innSrcSize || row + i >= outSrcSize) {
          dst(i)(j) = 0
        } else {
          dst(i)(j) = src(row + i)(col + j)
        }
      }
    }
  }
  
  // Copy from smaller array to a bigger one
  // row - destination row
  // col - destination col
  def writeSubArray(src: Array[Array[Int]], dst: Array[Array[Int]], 
                   row: Int, col: Int) = {
    val outSrcSize = src.length
    require(outSrcSize > 0)
    val innSrcSize = src(0).length
    require(innSrcSize > 0)
    val outDstSize = dst.length
    require(outDstSize > 0)
    val innDstSize = dst(0).length
    require(innDstSize > 0)
    
    val width  = innSrcSize
    val height = outSrcSize
    
    //println(s"-D- memWriteAcc: write row=$row col=$col")
    for (i <- 0 until height) {
      for (j <- 0 until width) {
        if (col + j >= innDstSize || row + i >= outDstSize) {
          require(src(i)(j) == 0, s"-F- Padded area must not accumulate values.")
        } else {
          dst(row + i)(col + j) = src(i)(j)
        }
      }
    }
  }
  
  // split data into bytes
  def splitData(data:BigInt, size:Int) = {
    val array = new scala.collection.mutable.ListBuffer[Int]
    val byteMask = (BigInt(1) << 8) - 1
    var memDada = data
    for (idx <- 0 until size) {
      val writeData = (memDada & byteMask).toInt
      memDada = memDada >> 8
      array.append(writeData)
      //println(s"-D- splitData: idx=$idx val=${BigInt(writeData).toString(2)}")
    }
    array
  }
  // split data into bytes and store in memory
  def addAsBytes(memory: Array[Int], memIdx: Int, dataBytes: Int, data:BigInt) = {
    //println(s"-D- addAsBytes: memIdx=$memIdx bytes=${dataBytes}")
    val res = splitData(data, dataBytes)
    for (idx <- 0 until res.length) {  
      memory(memIdx + idx) = res(idx)
    }
  }
  
  def dutCoreReaderState(c: Core) = {
    // (<vme channel index>,<name>)
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
    (readersState, writersState)
  }
  // GOLDEN reference GEMM
  def gemmRef(inp: Array[Int], wgt: Array[Int], accum: Array[Int], commonSize: Int, inpSize: Int, wgtSize: Int) : Array[Int] = {

    val res = Array.fill(inpSize * wgtSize) {0}
    for (i <- 0 until inpSize) {
      //println(s"a:${inp.slice(i*commonSize, (i+1)*commonSize).mkString(" ")}")
    }
    for (i <- 0 until wgtSize) {
      //println(s"b:${wgt.slice(i*commonSize, (i+1)*commonSize).mkString(" ")}")
    }
    for (i <- 0 until inpSize) {
      for (j <- 0 until wgtSize) {
        var dot = 0
        for (k <- 0 until commonSize) {
          dot += wgt(j*commonSize+k) * inp(i*commonSize+k)
        }
        //println(s"$i $j \na:${inp.slice(i*commonSize, (i+1)*commonSize).mkString(" ")}\nb:${wgt.slice(j*commonSize, (j+1)*commonSize).mkString(" ")}= $dot")
        res(i*wgtSize+j) = (dot + accum(i*wgtSize+j))%(1<<p(CoreKey).outBits)
      }
    }
    for (i <- 0 until inpSize) {
      //println(s"c:${res.slice(i*wgtSize, (i+1)*wgtSize).mkString(" ")}")
    }
    return res
  }
  // plain vector to commom width rectangular matrix convertion
  def tensorize(vector: Array[Int] /* input matrix */, width: Int /* tensor width */, 
                height: Int /* tensor height */, common:Int /* matrix width */, 
                tr: Boolean = false /* transpose */) : Array[Int] = {
    require(height > 0, "-F- Probably wrong VTA configuration for this test")
    require(width > 0, "-F- Probably wrong VTA configuration for this test")
    
    val size = vector.length
    val res = Array.fill(vector.length) {0}
    val tensorsInRow = ceil(common / width.toFloat).toInt
    val tensorsInCol = ceil(size / common / height.toFloat ).toInt
    
    //println(s"-D- tesorize Matrix len=${size} width=${common} tensor width=${width} height=${height} tr=${tr}")
    
    for (i <- 0 until size) {
      val matrixRow = i / common
      val matrixCol = i % common
      
      val tenzorRow = matrixRow % height
      val tensorIdxR = matrixRow / height
      val tensorCol = matrixCol % width
      val tensorIdxC = matrixCol / width
      val tensorOffet = if (tr) {
        tensorIdxC * tensorsInCol * width * height + tensorIdxR * (width*height)
      } else {
        tensorIdxR * tensorsInRow * width * height + tensorIdxC * (width*height)
      }
      val destIdx = tensorOffet + tenzorRow * width + tensorCol
      //println(s"-D- tesorize ${i}->${destIdx} tensor row=${tenzorRow}:${tensorIdxR} column=${tensorCol}:${tensorIdxC} matrix row=${matrixRow} column=${matrixCol} size=${size}  tensorOffet=${tensorOffet} tensorsInRow=${tensorsInRow} tensorsInCol=${tensorsInCol}")
      res(destIdx) = vector(i)
    }
    res
  }
  def readInstructionsBlock(memory: Array[Int] /* memory */, offset: Int, data: List[Any]) = {
    val m = data.asInstanceOf[List[Map[String, Any]]]
    val instBytes = INST_BITS / 8 // from ISA.scala
    
    for(idx <- 0 until m.length) {
      val instrData =m(idx)
      val iType = getInstrType(instrData)
      val data = iType match {
        case "CompGemReset" => gemInst(instrData, rst = true)
        case "CompGem" => gemInst(instrData)
        case "StorSnc" => memInst(instrData, tsize = 0/* xsize == 0 idetifies sync */, instOp = taskId("store"))
        case "Stor" => memInst(instrData, instOp = taskId("store"))
        case "LoadSnc "=> memInst(instrData, instOp = taskId("load"), instId = memId("inp") /* may be wgt also*/, tsize = 0/* xsize == 0 idetifies sync */)
        case "LoadInp" => memInst(instrData, instOp = taskId("load"), instId = memId("inp"))
        case "LoadWgt" => memInst(instrData, instOp = taskId("load"), instId = memId("wgt"))
        case "CompSnc "=> memInst(instrData, instOp = taskId("load"), instId = memId("uop"), tsize = 0/* xsize == 0 idetifies sync */)
        case "CompUop" => memInst(instrData, instOp = taskId("load"), instId = memId("uop"))
        case "CompAcc" => memInst(instrData, instOp = taskId("load"), instId = memId("acc"))
        case "CompAlu" => aluInst(instrData)
        case "CompFin" => finInst(instrData)
        case _  => {
          require(false, s"-F- Unsupproted instruction type ${iType}")
          BigInt(0)
        }
      }
      addAsBytes(memory, memIdx = instBytes*idx + offset, dataBytes = instBytes, data)
    }
  }
  def readInstructions(memory: Array[Int] /* memory */, data: List[Any]) = {
    val m = data.asInstanceOf[List[Map[String, Any]]]
    
    for(idx <- 0 until m.length) {
      val instrBlock =m(idx)
      val offset = instrBlock("Offset").asInstanceOf[Int]
      if (offset < 0) {
        println(s"-W- Ignoring Instructions block $idx")
      } else {
        val data   = instrBlock("Data").asInstanceOf[List[Map[String, Any]]]
        readInstructionsBlock(memory, offset, data)
      }
    }
  }
  def readRawMemory(memory: Array[Int] /* memory */, data: List[Any]) = {
    val m = data.asInstanceOf[List[Map[String, Any]]]
    
    for(idx <- 0 until m.length) {
      val instrBlock =m(idx)
      val offset = instrBlock("Offset").asInstanceOf[Int]
      val data   = instrBlock("Data").asInstanceOf[List[Int]]
      readRawMemoryBlock(memory, offset, data)
    }
  }
  def readRawMemoryBlock(memory: Array[Int] /* memory */, offset: Int, data: List[Int]) = {
    for(idx <- 0 until data.length) {
      memory(offset + idx) = data(idx)
      //println(s"-D- readRawMemoryBlock : ${offset + idx} ${data(idx)}")
    }
  }
  def readDump(fileName: String) = {
    val bufferedSource = Source.fromFile(fileName)
    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val m = mapper.readValue[Map[String, Any]](bufferedSource.reader())
    bufferedSource.close
    
    val memSize = m("Size").asInstanceOf[Int]
    val memory = Array.fill(memSize){0}
    val start = m("Start").asInstanceOf[Int]
    val instrCount = m("InstrCount").asInstanceOf[Int]
    if (m contains "RawMemory") {
      readRawMemory(memory, m("RawMemory").asInstanceOf[List[Any]])
    }
    if (loadInstructions && (m contains "Instructions")) {
      readInstructions(memory, m("Instructions").asInstanceOf[List[Any]])
    }
    (memory, start, instrCount)
  }
  def getInstrType(instrData: Map[String, Any]) = {
    if (instrData contains "TYPE") instrData("TYPE") else {
      val op = instrData("op")
      val id = instrData("id")
      if (       op == "finish"){ "CompFin"
      } else if (op == "store") { "Stor"
      } else if (op == "gemm")  { "CompGem"
      } else if (op == "alu")   { "CompAlu"
      } else if (op == "load")  {
          if (       id == "uop") { "CompUop"
          } else if (id == "acc") { "CompAcc"
          } else if (id == "inp") { "LoadWgt"
          } else if (id == "wgt") { "LoadInp"
          } else {
            require(false, s"-F- Unsupproted instruction id ${instrData("id")}")
            ""
          }
      } else {
        require(false, s"-F- Unsupproted instruction op ${instrData("op")}")
        ""
      }
    }
  }
  
  // set address to start execution from
  def setStart(instrStart: Int, instrNb: Int) = {memInstrStart = instrStart; memInstrNb = instrNb}
  def setAssertWrites(aw: Boolean) = {assertWrites = aw}
   
  def end() = {
    if (dumpMemory) {
      val fd = new PrintWriter(new File(dumpMemoryFileName))
      fd.write("{\n")
      fd.write(s""""Size" : ${dumpMemSize},  \n""")
      fd.write(s""""Start" :${memInstrStart},  \n""")
      fd.write(s""""InstrCount" :${memInstrNb},  \n""")
      for (idx <- 0 until memStr.length) {
        fd.write(memStr(idx))
      }
      fd.write("]\n")
      for (idx <- 0 until memInstrStr.length) {
        fd.write(memInstrStr(idx))
      }
      fd.write("]\n")
      fd.write("}]")
      fd.write("}")
      
      
      fd.close

    }
  }
  
  def genVTACommands(memory: Array[Int], cmdBeginDram: Int, instCount: Int, instBeginDram: Int) = {
    require(mp.addrBits <= 32, "-F- Change indexing to BigInt")
    val ptrScale = if (mp.addrBits == 32 ) 1 else 2
    val cmdNb = vp.nCtrl + vp.nECnt + vp.nVals + vp.nPtrs * ptrScale
    val cmdSizeByte = vp.regBits / 8
    val eo = vp.nCtrl
    val vo = eo + vp.nECnt
    val po = vo + vp.nVals
    val uo = po + vp.nPtrs * ptrScale
    
    for (idx <- 0 until cmdNb * cmdSizeByte) {
      memory(cmdBeginDram + idx) = 0
    }
    memory(cmdBeginDram) = 1 // launch cmd
    
    //set instructions count. 0th vo reg
    val instrCountData = splitData(instCount, cmdSizeByte)
    for (idx <- 0 until instrCountData.length) {  
      memory(cmdBeginDram + vo*cmdSizeByte + 0*ptrScale*cmdSizeByte + idx) = instrCountData(idx)
    }
    
    //set instructions ptr. 0th po reg
    val instrPtrData = splitData(instBeginDram, ptrScale * cmdSizeByte)
    for (idx <- 0 until instrPtrData.length) {  
      memory(cmdBeginDram + po*cmdSizeByte + 0*ptrScale*cmdSizeByte + idx) = instrPtrData(idx)
    }
    
    //All other regs are zero
    
    
  }

}

