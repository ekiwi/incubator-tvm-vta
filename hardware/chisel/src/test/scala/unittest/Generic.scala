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

import chisel3._
import chisel3.util._
import vta.util.config._
import chisel3.iotesters._
import org.scalatest.flatspec.AnyFlatSpec
import vta.JSONPynqConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenericTest[T <: Module, P <: PeekPokeTester[T], C <: Parameters]( tag : String, dutFactory : (Parameters) => T, testerFactory : (T) => P) extends AnyFlatSpec with Matchers {

  implicit val p: Parameters = new JSONPynqConfig

  val arguments = Array(
    "--backend-name", "treadle",
    //"--backend-name", "vcs",
    //"--repl-seq-mem", "-c:Core:-o:Core.de1.mems.conf",
    //"--is-verbose",
    "--test-seed", "0"
    )

  behavior of tag
  it should "not have expect violations" in {
    chisel3.iotesters.Driver.execute( arguments, ()=> dutFactory(p))(testerFactory)    should be (true)
  }
}
