package bitnn

import scala.math._
import chisel3._
import chisel3.util._
import bitnn._

// A bias element represents a 
// trainiable constant that adds
// it's sign to the result of the row
class BiasElement(width: Int) extends Module {
  
  val io = IO(new Bundle {
    
    // gradIn is used for gradient calculations,
    // it represents the sign of the error w.r.t to the neuron output
    // or in our case the row of RowElements (and bias element if it's there)
    val gradIn = Input(SignedIO())
    
    // Mode represents whether the BiasElement is forward propagating
    // or backward propagating
    val mode = Input(Bool())
    
    // out stores the result of the forward propagation of the BiasElement
    val out = Output(SignedIO())
  })

  // For the system to work correctly,
  // the accumulator holds the current
  // value of the weight.
  // In the forward pass, only the sign
  // of the accumulator is used.
  val accumulator = RegInit(0.S(width.W))
  
  // If our accumulator is positive, and the BiasElement is in forward mode,
  // the BiasElement outputs a +1, and if the accumulator is negative,
  // the BiasElement outputs a -1
  out.p := (!mode) && (accumulator > 0)
  out.n := (!mode) && (accumulator <= 0)

  when (mode) {
    // For saturating addition we need to keep track of the minimum
    // and maximum values of the accumulator, to make sure they don't
    // wrap around. This is necessary for backprop to work!
    
    // these are the two's complement bounds
    val maxInt = pow(2, width - 1) - 1
    val minInt = -pow(2, width - 1)
    
    // now calculate the incremented value (accounting for saturation)
    ceil = Mux(accumulator === maxInt.U(width.W), accumulator, accumulator + 1)
    // now calculate the decremented value (accounting for saturation)
    floor = Mux(accumulator === minInt.U(width.W), accumulator, accumulator - 1)

    // update the accumulator with the error signal.
    when (io.gradIn.p) {
      // if the error was positive (we overshot)
      // decrement the accumulator
      accumulator := floor
    }.elseWhen (io.gradIn.n) {
      // if the error was negative (we undershot)
      // increment the accumulator
      accumulator := ceil
    }
  }

  // BiasElements do not propagate any error because they were not triggered by an input
  // so we don't have a grad out!

}
