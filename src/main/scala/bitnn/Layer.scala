package bitnn

import chisel3._
import chisel3.util._
import bitnn.

class Layer(inWidth: Int, outWidth: Int, neuronWidth: Int) extends Module {
  val io = IO(new Bundle {
    val inVec = Input(Vec(inWidth, Bool()))
    val outVec = Input(Vec(outWidth, Bool()))
    val gradIn = Input(Vec(outWidth, SignedIO()))
    val gradOut = Input(Vec(inWidth, SignedIO()))
    val mode = Input(Bool())
  })

  val posTable = VecInit.fill(outWidth, VecInit.fill(inWidth, 0.B))
  val negTable = VecInit.fill(outWidth, VecInit.fill(inWidth, 0.B))
  val gradPosTable = VecInit.fill(inWidth, VecInit.fill(outWidth, 0.B))
  val gradNegTable = VecInit.fill(inWidth, VecInit.fill(outWidth, 0.B))

  val mat = VecInit.tabulate(outWidth) { rowIndex =>
    val row = VecInit.tabulate(inWidth) { colIndex =>
      val neuron = Neuron(neuronWidth))
      neuron.io.fire := io.inVec(colIndex.U)
      neuron.io.out.p := posTable(rowIndex.U)(colIndex.U)
      neuron.io.out.n := negTable(rowIndex.U)(colIndex.U)
      neuron.io.gradIn := io.gradIn(rowIndex.U)
      neuron.io.gradOut.p := gradPosTable(rowIndex.U)(colIndex.U)
      neuron.io.gradOut.n := gradNegTable(rowIndex.U)(colIndex.U)
      neuron.io.mode := io.mode
      neuron
    }
    io.outVec(rowIndex.U) := posTable(rowIndex.U).count() > negTable(rowIndex.U).count()
    row
  }

  for (var colIndex <- 0 to inWidth) {
    val posCount = gradPosTable(colIndex.U).count()
    val negCount = gradNegTable(colIndex.U).count()
    io.gradOut(colIndex.U).p := posCount > negCount
    io.gradOut(colIndex.U).n := posCount < negCount
  }
}
