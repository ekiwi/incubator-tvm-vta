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
import vta.interface.axi.AXIParams
import vta.util.config._
import vta.shell._
import vta.verif.{TraceMgr => trace_mgr}

class TensorStoreIO(val mp: AXIParams, val tensorType: String)(implicit p: Parameters) extends Bundle {
  val start = Input(Bool())
  val done = Output(Bool())
  val inst = Input(UInt(INST_BITS.W))
  val baddr = Input(UInt(mp.addrBits.W))
  val vme_wr = new VMEWriteMaster
  val tensor = new TensorClient(tensorType)
}

trait IsTensorStore { val io: TensorStoreIO }


/** TensorStore.
 *
 * Store 1D and 2D tensors from out-scratchpad (SRAM) to main memory (DRAM).
 */
class TensorStore(tensorType: String = "none", debug: Boolean = false)(
    implicit p: Parameters)
    extends Module with IsTensorStore {
  val tp = new TensorParams(tensorType)
  val mp = p(ShellKey).memParams
  val io = IO(new TensorStoreIO(mp, tensorType))

  override def desiredName = "TensorStore" + tensorType.capitalize

  val tensorStore =
    Module( if (mp.dataBits >= tp.tensorSizeBits && !JSONFeatures.storeOld()) {
      // cacheline is wider than tensor size,
      // macro memory bitwidth by cache size
      // bank by tansor size
      new TensorStoreWideVME(tensorType, debug)
    } else {
      // tensor is wider than cacheline, bank by
      // macro memory bitwidth by tansor size
      // bank by cacheline size
      new TensorStoreNarrowVME(tensorType, debug)
    })

    io <> tensorStore.io
}
