import modele.GridSim
import hevs.graphics.FunGraphics

object Simu extends App {
  val width : Int = 100
  val height : Int = 100

  val initial = GridSim.randomGrid(width, height)

  val grids: Seq[GridSim] =
    Iterator.iterate(initial)(_.updateGrid()).take(20).toSeq

  grids.zipWithIndex.foreach { case (g, i) =>
    println(s"Step ${i + 1}:\n$g\n")
  }
}
