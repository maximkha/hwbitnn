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
      val rowelem = RoweElement(neuronWidth))
      rowelem.io.fire := io.inVec(colIndex.U)
      rowelem.io.out.p := posTable(rowIndex.U)(colIndex.U)
      rowelem.io.out.n := negTable(rowIndex.U)(colIndex.U)
      rowelem.io.gradIn := io.gradIn(rowIndex.U)
      rowelem.io.gradOut.p := gradPosTable(rowIndex.U)(colIndex.U)
      rowelem.io.gradOut.n := gradNegTable(rowIndex.U)(colIndex.U)
      rowelem.io.mode := io.mode
      rowelem
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
