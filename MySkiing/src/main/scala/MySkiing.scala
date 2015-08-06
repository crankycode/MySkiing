/**
 * Created by Li Zuyi
 */

import scala.io.Source

object MySkiing {

  // reading file
  val home = System.getProperty("user.dir")
  val source = Source.fromFile(s"$home/map.txt", "UTF-8")
  val lineIterator = source.getLines()
  var readInGraph :Vector[(Vector[Int])] = Vector[(Vector[Int])]()

  // row and col of graph
  var row: Int = 0
  var col: Int = 0

  // current best Path and Steepest found
  var bestPath: Seq[(Int,Int)] = Seq()

  // current best Path Drop
  var bestPathDrop = 0

  val graph2 = Vector(
    Vector(4,8,7,3),
    Vector(2,5,9,3),
    Vector(6,3,2,5),
    Vector(4,4,1,6))

  def main(args: Array[String]) = {

    // read in the graph file "map.txt"
    for (l <- lineIterator.drop(1)) {
      val myNewVector = l.split(" ").map(_.toInt).toVector
      readInGraph = readInGraph:+ myNewVector
    }

    // Start the search for best Path
    skiing(readInGraph)

    // Print out the result
    println("Found Path Coordinates: " + bestPath)
    println(bestPath.map(index => readInGraph(index._1)(index._2)).mkString("Best Path: ",", ",""))
    println("Best Path Length: " + bestPath.length)
    printf("Best Path Drop: %d",readInGraph(bestPath.head._1)(bestPath.head._2) - readInGraph(bestPath.last._1)(bestPath.last._2))

  }

  def skiing(graph: Vector[Vector[Int]]): Int = {

    // Main loop for the input graph
    if(row < graph.length) {
      if(col < graph(row).length) {
        travers(row,col,graph,graph(row)(col), Seq.apply((row,col)))
        col += 1
      }
      else {
        row += 1
        col = 0
      }
      skiing(graph)
    }
    else {
      row
    }
  }

  def travers (curRow: Int, curCol: Int, myGraph: Vector[Vector[Int]], myCurDrop: Int, myCurPath: Seq[(Int,Int)]):Seq[(Int,Int)]  = {

    // Search west side of current pos
    if (myGraph.lift(curRow).flatMap(_.lift(curCol - 1)).getOrElse(error()) < myGraph(curRow)(curCol)){
      val drop =myGraph(myCurPath.head._1)(myCurPath.head._2) - myGraph(curRow)(curCol - 1)
      val path = myCurPath :+ (curRow,curCol - 1)
        travers(curRow,curCol - 1, myGraph,drop, path)
    }

    // Search south side of current pos
    if (myGraph.lift(curRow + 1).flatMap(_.lift(curCol)).getOrElse(error()) < myGraph(curRow)(curCol)){
      val drop =myGraph(myCurPath.head._1)(myCurPath.head._2) - myGraph(curRow + 1)(curCol)
      val path = myCurPath :+ (curRow + 1,curCol)
      travers(curRow + 1,curCol, myGraph,drop, path)
    }

    // Search east side of current pos
    if (myGraph.lift(curRow).flatMap(_.lift(curCol + 1)).getOrElse(error()) < myGraph(curRow)(curCol)){
      val drop =myGraph(myCurPath.head._1)(myCurPath.head._2) - myGraph(curRow)(curCol + 1)
      val path = myCurPath :+ (curRow,curCol + 1)
      travers(curRow,curCol + 1, myGraph,drop, path)
    }

    // Search north side of current pos
    if (myGraph.lift(curRow - 1).flatMap(_.lift(curCol)).getOrElse(error()) < myGraph(curRow)(curCol)){
      val drop =myGraph(myCurPath.head._1)(myCurPath.head._2) - myGraph(curRow - 1)(curCol)
      val path = myCurPath :+ (curRow - 1,curCol)
      travers(curRow - 1,curCol, myGraph,drop, path)
    }
    else {
      // Finish search
      // Swap with found path, if current best path length is smaller than found path length
      if (bestPath.length < myCurPath.length) {
        bestPath = myCurPath
//        bestPathSteep = myCurSteep
        bestPathDrop = myGraph(myCurPath.head._1)(myCurPath.head._2) - myGraph(myCurPath.last._1)(myCurPath.last._2)
      }
      // Swap with found path, if current best length is the same as found path length but the drop is smaller
      else if (bestPath.length == myCurPath.length) {
        if (bestPathDrop < myCurDrop) {
          bestPath = myCurPath
          bestPathDrop = myCurDrop
        }
      }
      myCurPath
    }
  }

  def error(): Int = {
    // ArrayOutOfBound exception handling
    Int.MaxValue
  }
}
