package bitnn

import chisel3._
import chisel3.util._

// SignedIO represents either +1, 0, or -1
// This is used for the accumulation of neuron outputs
// as well as representing gradients
class SignedIO extends Bundle {
  val p = Bool()
  val n = Bool()
}