package modele

import scala.annotation.tailrec
import scala.util.Random

import java.io.{FileWriter,PrintWriter}

sealed trait Cell
case class Tree_Cell(temperature: Float) extends Cell
case class Fire_Cell(firestep : Int, temperature: Float) extends Cell
case class Empty_Cell(temperature: Float) extends Cell
case class Water_Cell(temperature : Float) extends Cell

case class StepStats(step: Int, trees: Int, fires: Int)
case class SimulationResult(fd: Float, runs: Seq[Seq[StepStats]])

case class GridSim(w: Int, h: Int, grid: Vector[Vector[Cell]], T : Float = 20.0f) {

  def countCellTypes(): (Int, Int) = {
    val allCells = grid.flatten
    val treeCount = allCells.count(_.isInstanceOf[Tree_Cell])
    val fireCount = allCells.count(_.isInstanceOf[Fire_Cell])
    (treeCount, fireCount)
  }

  def updateCell(i: Int, j: Int): Cell = {
    grid(i)(j) match {
      case Fire_Cell(f,t) => if (f>5) Empty_Cell(T) else Fire_Cell(f+1,t)
      case Tree_Cell(t) =>
        val intensitySum = neighborFireIntensity(i,j)
        val prob = 1 - math.exp(-intensitySum)
        if (Random.nextFloat()<prob) Fire_Cell(0,255.0f) else Tree_Cell((intensitySum+t)/9)
      case Empty_Cell(t) => Empty_Cell(t)
      case Water_Cell(t) => Water_Cell(t)
    }
  }

  def neighborFireIntensity(i: Int, j: Int): Int = {
    val directions = for {
      di <- -1 to 1
      dj <- -1 to 1
      if !(di == 0 && dj == 0)
    } yield (i + di, j + dj)

    directions.collect {
      case (ni, nj) if ni >= 0 && ni < h && nj >= 0 && nj < w =>
        grid(ni)(nj) match {
          case Fire_Cell(f, _) => f
          case _ => 0
        }
    }.sum
  }



  def updateGrid(): GridSim = {
    val newGrid = Vector.tabulate(h, w)((i, j) => updateCell(i, j))
    GridSim(w, h, newGrid)
  }

  /*override def toString: String = {
    grid.map { row =>
      row.map {
        case Tree_Cell()  => "T"
        case Fire_Cell(a)  => "F"
        case Empty_Cell() => "."
      }.mkString
    }.mkString("\n")
  }*/
}

object GridSim {
  def randomGrid(w: Int, h: Int,fD: Float, T: Float, nbLakes: Int = 3, lakeAreaMin: Int = 200, lakeAreaMax: Int = 300): GridSim = {
    val rand = new scala.util.Random

    def generateLakes(): Set[(Int, Int)] = {
      var waterCells = Set.empty[(Int, Int)]

      def neighbors(x: Int, y: Int): Seq[(Int, Int)] =
        Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).filter {
          case (nx, ny) => nx >= 0 && nx < w && ny >= 0 && ny < h
        }

      def dilate(cells: Set[(Int, Int)]): Set[(Int, Int)] = {
        cells.flatMap { case (x, y) =>
          neighbors(x, y).filter {
            case (nx, ny) =>
              !cells.contains((nx, ny)) && rand.nextFloat() < 0.6
          }
        } ++ cells
      }

      def fillHoles(cells: Set[(Int, Int)]): Set[(Int, Int)] = {
        val allCoords = (for (x <- 0 until w; y <- 0 until h) yield (x, y)).toSet
        val land = allCoords -- cells

        // Toutes les cases accessibles depuis le bord
        var reachable = Set.empty[(Int, Int)]
        var frontier = (0 until w).flatMap(x => Seq((x, 0), (x, h - 1))).toSet ++
          (0 until h).flatMap(y => Seq((0, y), (w - 1, y))).toSet

        frontier = frontier.intersect(land)
        reachable ++= frontier

        while (frontier.nonEmpty) {
          val newFrontier = frontier.flatMap { case (x, y) =>
            neighbors(x, y).filter(p => land.contains(p) && !reachable.contains(p))
          }
          reachable ++= newFrontier
          frontier = newFrontier
        }

        val holes = land -- reachable
        cells ++ holes // on transforme les trous en eau
      }

      for (_ <- 1 to nbLakes) {
        val start = (rand.nextInt(w), rand.nextInt(h))
        var toExpand = Set(start)
        var lake = Set(start)

        while (lake.size < lakeAreaMax && toExpand.nonEmpty) {
          val newPoints = toExpand.flatMap { case (x, y) =>
            neighbors(x, y).filter {
              case (nx, ny) =>
                !lake.contains((nx, ny)) && rand.nextFloat() < 0.5
            }
          }

          toExpand = newPoints
          lake ++= newPoints
        }

        if (lake.size >= lakeAreaMin) {
          val filledLake = fillHoles(lake)
          val softenedLake = (1 to 5).foldLeft(filledLake)((acc,_) => dilate(acc))

          waterCells ++= softenedLake
        }
      }

      waterCells
    }

    @tailrec
    def generateFirePosition(lakeCells: Set[(Int, Int)]): (Int, Int) = {
      val pos = (rand.nextInt(w), rand.nextInt(h))
      if (lakeCells.contains(pos)) generateFirePosition(lakeCells)
      else pos
    }

    val lakeCells: Set[(Int, Int)] = generateLakes()
    val (fireX, fireY) = generateFirePosition(lakeCells)

    def defineCell(i: Int, j: Int): Cell = {
      if ((j, i) == (fireX, fireY))
        Fire_Cell(0,1000.0f)
      else if (lakeCells.contains((j, i)))
        Water_Cell(T)
      else {
        if (Random.nextFloat() > fD) Empty_Cell(20.0f) else Tree_Cell(20.0f)
      }
    }

    val grid = Vector.tabulate(h, w)(defineCell)
    GridSim(w, h, grid)
  }

}



