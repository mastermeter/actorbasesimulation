import modele._

import scala.util.Random
import java.io.{FileWriter, PrintWriter}

object SimuStats extends App {

  def writeSimulationsToJsonFile(results: Seq[SimulationResult], filePath: String): Unit = {
    val writer = new PrintWriter(new FileWriter(filePath))
    writer.println("[")

    results.zipWithIndex.foreach { case (res, fdIdx) =>
      writer.println(s"  { \"fd\": ${res.fd}, \"runs\": [")

      res.runs.zipWithIndex.foreach { case (runStats, runIdx) =>
        writer.println("    [")
        runStats.zipWithIndex.foreach { case (step, sIdx) =>
          val stepLine = s"      { \"step\": ${step.step}, \"trees\": ${step.trees}, \"fires\": ${step.fires} }"
          if (sIdx < runStats.length - 1) writer.println(stepLine + ",")
          else writer.println(stepLine)
        }
        writer.print("    ]")
        if (runIdx < res.runs.length - 1) writer.println(",") else writer.println()
      }

      writer.print("  ]}")
      if (fdIdx < results.length - 1) writer.println(",") else writer.println()
    }

    writer.println("]")
    writer.close()
  }

  val step = 300
  val w = 100
  val h = 100
  val T = 20.0f
  val numRunsPerFd = 10

  var allSimulations = Vector.empty[SimulationResult]

  for (fd <- 0 to 100) {
    println(f"fd = ${fd / 100.0f}%.2f")

    val fdFloat = fd / 100.0f
    val allRuns = for (run <- 1 to numRunsPerFd) yield {
      var grid: GridSim = GridSim.randomGrid(w, h, fdFloat, T)
      var stats = Vector.empty[StepStats]

      var i = 0
      var done = false

      while (i <= step && !done) {
        val (trees, fires) = grid.countCellTypes()
        if (i == 0 || i == step || fires == 0)
          stats :+= StepStats(i, trees, fires)

        if (fires == 0) {
          done = true
        } else {
          grid = grid.updateGrid()
          i += 1
        }
      }

      println(s"  Run $run terminé à step $i")
      stats
    }

    allSimulations :+= SimulationResult(fdFloat, allRuns)
  }

  writeSimulationsToJsonFile(allSimulations, "stats.json")
  println("Fini")
}
