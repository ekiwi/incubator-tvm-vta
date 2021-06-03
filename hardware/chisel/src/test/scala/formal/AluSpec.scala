// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package formal

import chisel3._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import paso._
import unittest.util.helper
import vta.JSONPynqConfig
import vta.core.AluVector
import vta.util.config.Parameters

class AluArgs(bitWidth: Int, vectorLength: Int) extends Bundle {
  val opcode = UInt(3.W)
  val a = Vec(vectorLength, SInt(bitWidth.W))
  val b = Vec(vectorLength, SInt(bitWidth.W))
}

class AluModel(width: Int, size: Int) extends UntimedModule {
  val exec = fun("exec").in(new AluArgs(width, size)).out(Vec(size, SInt(width.W))) { (in, res) =>
    val widthBits = log2Ceil(width)
    val mask = helper.getMask(widthBits)
    when(in.opcode === 0.U) {
      for (i <- 0 until size) {
        res(i) := Mux(in.a(i) < in.b(i), in.a(i), in.b(i))
      }
    } .elsewhen(in.opcode === 1.U) {
      for (i <- 0 until size) {
        res(i) := Mux(in.a(i) < in.b(i), in.b(i), in.a(i))
      }
    } .elsewhen(in.opcode === 2.U) {
      for (i <- 0 until size) {
        res(i) := in.a(i) + in.b(i)
      }
    } .elsewhen(in.opcode === 3.U) {
      for (i <- 0 until size) {
        res(i) := in.a(i) >> (in.b(i).asUInt & mask.U)(widthBits-1, 0)
      }
    } .elsewhen(in.opcode === 7.U) {
      for (i <- 0 until size) {
        res(i) := in.a(i) << ((-1.S * in.b(i)).asUInt & mask.U)(widthBits-1, 0)
      }
    } .elsewhen(in.opcode === 4.U) {
      for (i <- 0 until size) {
        when(in.a(i) < (-1.S * in.b(i))) {
          res(i) := -1.S * in.b(i)
        } .elsewhen(in.a(i) > in.b(i)) {
          res(i) := in.b(i)
        } .otherwise {
          res(i) := in.a(i)
        }
      }
    } .elsewhen(in.opcode === 5.U) {
      for (i <- 0 until size) {
        res(i) := in.b(i)
      }
    } .elsewhen(in.opcode === 6.U) {
      for (i <- 0 until size) {
        // TODO: why does the original cast toByte ?
        res(i) := in.a(i) * in.b(i)
      }
    } .otherwise {
      for (i <- 0 until size) {
        res(i) := 0.S
      }
    }
  }
}

class AluProtocol(impl: AluVector) extends ProtocolSpec[AluModel] {
  override val spec = new AluModel(width = impl.aluBits, size = impl.blockOut)

  protocol(spec.exec)(impl.io) { (clock, io, in, res) =>
    for (i <- 0 until impl.blockOut) {
      io.acc_a.data.bits(0)(i).poke(in.a(i).asUInt)
      io.acc_b.data.bits(0)(i).poke(in.b(i).asUInt)
    }
    io.opcode.poke(in.opcode)

    io.acc_a.data.valid.poke(true.B)
    io.acc_b.data.valid.poke(true.B)

    clock.step()

    io.acc_a.data.valid.poke(false.B)
    io.acc_b.data.valid.poke(false.B)

    // wait for valid signal
    do_while(!io.acc_y.data.valid, 10) {
      clock.step() // advance clock
    }

    for (i <- 0 until impl.blockOut) {
      io.acc_y.data.bits(0)(i).expect(res(i).asUInt)
    }
    clock.step()
  }
}

class AluSpec extends AnyFlatSpec with PasoTester {
  behavior of "AluVector"

  val p: Parameters = new JSONPynqConfig

  it should "pass random testing" in {
    test(new AluVector()(p))(new AluProtocol(_)).randomTest(10, recordWaveform = true, seed = Some(48))
  }

  it should "pass bmc" in {
    test(new AluVector()(p))(new AluProtocol(_)).bmc(3)
  }
}