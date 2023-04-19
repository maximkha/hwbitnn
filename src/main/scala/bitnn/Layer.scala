package bitnn

import chisel3._
import chisel3.util._
import bitnn._

class Layer(inWidth: Int, outWidth: Int, accumulatorWidth: Int) extends Module {
  val io = IO(new Bundle {
    val inVec = Input(Vec(inWidth, Bool()))
    val outVec = Input(Vec(outWidth, Bool()))
    val gradIn = Input(Vec(outWidth, SignedIO()))
    val gradOut = Input(Vec(inWidth, SignedIO()))
    val mode = Input(Bool())
  })

  // For the matrix, we have to add one more column
  // corresponding to the bias element
  val posTable = VecInit.fill(outWidth, VecInit.fill(inWidth+1, 0.B))
  val negTable = VecInit.fill(outWidth, VecInit.fill(inWidth+1, 0.B))
  val gradPosTable = VecInit.fill(inWidth, VecInit.fill(outWidth, 0.B))
  val gradNegTable = VecInit.fill(inWidth, VecInit.fill(outWidth, 0.B))

  val mat = VecInit.tabulate(outWidth) { rowIndex =>
    val biaselem = BiasElement(accumulatorWidth)
    
    val row = VecInit.tabulate(inWidth) { colIndex =>
      val rowelem = RoweElement(accumulatorWidth)
      rowelem.io.fire := io.inVec(colIndex.U)
      rowelem.io.out.p := posTable(rowIndex.U)(colIndex.U)
      rowelem.io.out.n := negTable(rowIndex.U)(colIndex.U)
      rowelem.io.gradIn := io.gradIn(rowIndex.U)
      rowelem.io.gradOut.p := gradPosTable(rowIndex.U)(colIndex.U)
      rowelem.io.gradOut.n := gradNegTable(rowIndex.U)(colIndex.U)
      rowelem.io.mode := io.mode
      rowelem
    }

    // TODO check this
    biaselem.out.p := posTable(rowIndex.U)(inWidth)
    biaselem.out.n := negTable(rowIndex.U)(inWidth)

    io.outVec(rowIndex.U) := posTable(rowIndex.U).count() > negTable(rowIndex.U).count()

    row
  }

  var colIndex = 0
  for (colIndex <- 0 to inWidth) {
    val posCount = gradPosTable(colIndex.U).count()
    val negCount = gradNegTable(colIndex.U).count()

    io.gradOut(colIndex.U).p := posCount > negCount
    io.gradOut(colIndex.U).n := posCount < negCount
  }
}
