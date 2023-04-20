package bitnn

import scala.math._
import chisel3._
import chisel3.util._
import bitnn._

// Represents a row element in a matrix
// each row represents a neuron.
class RowElement(width: Int) extends Module {
  // A normal neuron works like: f(sum(x_i*w_i)), 
  // where f is a non linear activation function
  
  // A row element takes in a fire signal
  // (whether to contribute its weight) to the row accumulator
  // as a optimization (also how it's implented in bitnn.cpp), the 
  // summation and activation function (in our case the step function) calculations are fused.
  
  val io = IO(new Bundle {
    // Whether the input to the row element was a 1, or 0
    val fire = Input(Bool())
    
    // gradIn is used for gradient calculations,
    // it represents the sign of the error w.r.t to the neuron output
    // or in our case the row of RowElements
    val gradIn = Input(new SignedIO())
    
    // Mode represents whether the RowElement is forward propagating
    // or backward propagating
    val mode = Input(Bool())
    
    // gradOut is the resulting error of the input
    val gradOut = Output(new SignedIO())
    
    // out stores the result of the forward propagation of the RowElement
    val out = Output(new SignedIO())
  })

  // For the system to work correctly,
  // the accumulator holds the current
  // value of the weight.
  // In the forward pass, only the sign
  // of the accumulator is used.
  val accumulator = RegInit(0.S(width.W))
  
  // If our accumulator is positive, and the RowElement fired,
  // the RowElement outputs a +1, and if the accumulator is negative,
  // the RowElement outputs a -1
  io.out.p := io.fire && (accumulator > 0.S)
  io.out.n := io.fire && (accumulator <= 0.S)
  
  // For backward propigation,
  // we need to store whether the RowElement
  // fired / recieved an input
  val didFire = RegInit(0.B)
  when (!io.mode) {
    didFire := fire
  }

  when (io.mode && didFire) {
    // For saturating addition we need to keep track of the minimum
    // and maximum values of the accumulator, to make sure they don't
    // wrap around. This is necessary for backprop to work!
    
    // these are the two's complement bounds
    val maxInt = pow(2, width - 1) - 1
    val minInt = -pow(2, width - 1)
    
    // now calculate the incremented value (accounting for saturation)
    val saturated_increment = Mux(accumulator === maxInt.U(width.W), accumulator, accumulator + 1.S)
    // now calculate the decremented value (accounting for saturation)
    val saturated_decrement = Mux(accumulator === minInt.U(width.W), accumulator, accumulator - 1.S)

    // update the accumulator with the error signal.
    when (io.gradIn.p) {
      // if the error was positive (we overshot)
      // decrement the accumulator
      accumulator := saturated_decrement
    }.elsewhen (io.gradIn.n) {
      // if the error was negative (we undershot)
      // increment the accumulator
      accumulator := saturated_increment
    }
  }

  // maybe combine this with the condition aboce
  when (io.mode)
  {
    // propagate error
    when (io.gradIn.p) {
      // +1 * +1 = +1
      // +1 * -1 = -1
      io.gradOut.p = (accumulator > 0.S)
      io.gradOut.n = (accumulator < 0.S)
    }.elsewhen (io.gradIn.n) {
      // -1 * +1 = -1
      // -1 * -1 = +1

      io.gradOut.p = (accumulator < 0.S)
      io.gradOut.n = (accumulator > 0.S)
    }.otherwise {
      io.gradOut.p = false.B
      io.gradOut.n = false.B
    }
    // io.gradOut.p = (io.gradIn.n ^ (accumulator > 0)) & (io.gradIn.n | io.gradIn.p)
    // io.gradOut.n = (io.gradIn.n ^ (accumulator < 0)) & (io.gradIn.n | io.gradIn.p)
  }
}
