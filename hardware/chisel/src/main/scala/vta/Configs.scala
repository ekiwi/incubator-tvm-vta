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

package vta

import chisel3._
import vta.util.config._
import vta.shell._
import vta.core._
import vta.dpi._
import vta.test._

/** VTA.
 *
 * This file contains all the configurations supported by VTA.
 * These configurations are built in a mix/match form based on core
 * and shell configurations.
 */
class DefaultPynqConfig extends Config(new CoreConfig ++ new PynqConfig)// ++ new DpiConfig)
class JSONPynqConfig extends Config(new JSONCoreConfig ++ new JSONShlPynqConfig)// ++ new JSONDpiConfig)
class DefaultF1Config extends Config(new CoreConfig ++ new F1Config)// ++ new DpiConfig)
class DefaultDe10Config extends Config(new CoreConfig ++ new De10Config)// ++ new DpiConfig)
class JSONDe10Config extends Config(new JSONCoreConfig ++ new JSONShlDe10Config)// ++ new JSONDpiConfig)
class JSONMinimalConfig extends Config(new JSONCoreConfig ++ new MinimalConfig)// ++ new DpiConfig)

object DefaultPynqConfig extends App {
  implicit val p: Parameters = new DefaultPynqConfig
  chisel3.Driver.execute(args, () => new XilinxShell)
}

object JSONPynqConfig extends App {
  implicit val p: Parameters = new JSONPynqConfig
  chisel3.Driver.execute(args, () => new XilinxShell)
}

object DefaultF1Config extends App {
  implicit val p: Parameters = new DefaultF1Config
  chisel3.Driver.execute(args, () => new XilinxShell)
}

object DefaultDe10Config extends App {
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new IntelShell)
}

object JSONDe10Config extends App {
  implicit val p: Parameters = new JSONDe10Config
  chisel3.Driver.execute(args, () => new IntelShell)
}

object JSONMinimalConfig extends App {
  implicit val p: Parameters = new JSONMinimalConfig
  chisel3.Driver.execute(args, () => new IntelShell)
}

object TestDefaultPynqConfig extends App {
  implicit val p: Parameters = new DefaultPynqConfig
  chisel3.Driver.execute(args, () => new Test)
}

object TestJSONPynqConfig extends App {
  implicit val p: Parameters = new JSONPynqConfig
  chisel3.Driver.execute(args, () => new Test)
}

object TestDefaultF1Config extends App {
  implicit val p: Parameters = new DefaultF1Config
  chisel3.Driver.execute(args, () => new Test)
}

object TestDefaultDe10Config extends App {
  implicit val p: Parameters = new DefaultDe10Config
  chisel3.Driver.execute(args, () => new Test)
}

object TestJSONDe10Config extends App {
  implicit val p: Parameters = new JSONDe10Config
  chisel3.Driver.execute(args, () => new Test)
}

object TestJSONMinimalConfig extends App {
  implicit val p: Parameters = new JSONMinimalConfig
  chisel3.Driver.execute(args, () => new Test)
}
