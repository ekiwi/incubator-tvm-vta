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

package vta.util.config

import scala.io.Source
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

// taken from https://github.com/vta.roject/rocket-chip

abstract class Field[T] private (val default: Option[T]) {
  def this() = this(None)
  def this(default: T) = this(Some(default))
}

abstract class View {
  final def apply[T](pname: Field[T]): T = apply(pname, this)
  final def apply[T](pname: Field[T], site: View): T = {
    val out = find(pname, site)
    require(out.isDefined, s"Key ${pname} is not defined in Parameters")
    out.get
  }

  final def lift[T](pname: Field[T]): Option[T] = lift(pname, this)
  final def lift[T](pname: Field[T], site: View): Option[T] =
    find(pname, site).map(_.asInstanceOf[T])

  protected[config] def find[T](pname: Field[T], site: View): Option[T]
}

abstract class Parameters extends View {
  final def ++(x: Parameters): Parameters =
    new ChainParameters(this, x)

  final def alter(
    f: (View, View, View) => PartialFunction[Any, Any]): Parameters =
    Parameters(f) ++ this

  final def alterPartial(f: PartialFunction[Any, Any]): Parameters =
    Parameters((_, _, _) => f) ++ this

  final def alterMap(m: Map[Any, Any]): Parameters =
    new MapParameters(m) ++ this

  protected[config] def chain[T](site: View,
    tail: View,
    pname: Field[T]): Option[T]
  protected[config] def find[T](pname: Field[T], site: View) =
    chain(site, new TerminalView, pname)
}

object Parameters {
  def empty: Parameters = new EmptyParameters
  def apply(f: (View, View, View) => PartialFunction[Any, Any]): Parameters =
    new PartialParameters(f)
}

class Config(p: Parameters) extends Parameters {
  def this(f: (View, View, View) => PartialFunction[Any, Any]) =
    this(Parameters(f))

  protected[config] def chain[T](site: View, tail: View, pname: Field[T]) =
    p.chain(site, tail, pname)
  override def toString = this.getClass.getSimpleName
  def toInstance = this
}

// Internal implementation:

private class TerminalView extends View {
  def find[T](pname: Field[T], site: View): Option[T] = pname.default
}

private class ChainView(head: Parameters, tail: View) extends View {
  def find[T](pname: Field[T], site: View) = head.chain(site, tail, pname)
}

private class ChainParameters(x: Parameters, y: Parameters) extends Parameters {
  def chain[T](site: View, tail: View, pname: Field[T]) =
    x.chain(site, new ChainView(y, tail), pname)
}

private class EmptyParameters extends Parameters {
  def chain[T](site: View, tail: View, pname: Field[T]) = tail.find(pname, site)
}

private class PartialParameters(
    f: (View, View, View) => PartialFunction[Any, Any])
    extends Parameters {
  protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = {
    val g = f(site, this, tail)
    if (g.isDefinedAt(pname)) Some(g.apply(pname).asInstanceOf[T])
    else tail.find(pname, site)
  }
}

private class MapParameters(map: Map[Any, Any]) extends Parameters {
  protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = {
    val g = map.get(pname)
    if (g.isDefined) Some(g.get.asInstanceOf[T]) else tail.find(pname, site)
  }
}

// Load json
object JSONConfig {
  val vta_config = System.getenv("VTA_CONFIG")
  val file = if (vta_config == null) "vta_config" else vta_config
  val filename = "../../config/" + file + ".json"
  print(f"Populating JSONConfig parameters from JSON file: $filename\n")
  val bufferedSource = Source.fromFile(filename)
  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  val m = mapper.readValue[Map[String, Any]](bufferedSource.reader())
  bufferedSource.close
}
// global json options:
object JSONFeatures {
  def vmeOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_VME") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
  def tensorLoadOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_TENSOR_LOAD") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
  def fetchOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_FETCH") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
  def loadUopOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_LOAD_UOP") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
  def storeOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_TENSOR_STORE") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
  def queuesOld() = {
    val default_result = false
    JSONConfig.m.get( "VTA_FEATURE_ORIGINAL_QUEUES") match {
      case None => default_result
      case Some(x) =>
        val result = x.asInstanceOf[Boolean]
        result
    }
  }
}

case class VerifParams(trace: Boolean)
case object VerifKey extends Field[VerifParams]
