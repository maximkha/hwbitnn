package bitnn

import scala.math._
import chisel3._
import chisel3.util._

class SignedIO extends Bundle {
  val p = Bool()
  val n = Bool()
}

class Neuron(width: Int) extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val gradIn = Input(SignedIO())
    val mode = Input(Bool())
    val gradOut = Output(SignedIO())
    val out = Output(SignedIO())
  })

  val accumulator = RegInit(0.S(width.W))
  out.p := fire && (accumulator > 0)
  out.n := fire && (accumulator <= 0)
  
  val didFire = RegInit(0.B)
  when (!mode) {
    didFire := fire
  }

  when (mode && didFire) {
    val maxInt = pow(2, width - 1) - 1
    val minInt = -pow(2, width - 1)
    ceil = Mux(accumulator === maxInt.U(width.W), accumulator, accumulator + 1)
    floor = Mux(accumulator === minInt.U(width.W), accumulator, accumulator - 1)

    when (io.gradIn.p) {
      accumulator := floor
    }.elseWhen (io.gradIn.n) {
      accumulator := ceil
    }
  }
}
