import scalaz.Scalaz._

import scala.io.Source

object Day10 {
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def isVisible(map: List[String], fromX: Int, fromY: Int, toX: Int, toY: Int): Boolean = {
    val pos = (toX - fromX, toY - fromY)
    val divider = gcd(Math.abs(pos._1), Math.abs(pos._2))
    val vector = (pos._1 / divider, pos._2 / divider)
    var currentPos = vector
    while (fromX + currentPos._1 != toX || fromY + currentPos._2 != toY) {
      if (map(fromY + currentPos._2)(fromX + currentPos._1) == '#') {
        return false
      }
      currentPos = currentPos |+| vector
    }
    true
  }

  def countVisible(map: List[String], fromX: Int, fromY: Int): Int = {
    if (map(fromY)(fromX) != '#') {
      return 0
    }
    var counter = 0
    for {
      toY <- map.indices
      toX <- map.indices
    } {
      if (toX != fromX || toY != fromY) {
        if (map(toY)(toX) == '#') {
          if (isVisible(map, fromX, fromY, toX, toY)) {
            counter += 1
          }
        }
      }
    }
    counter
  }

  def main(args: Array[String]) {
    // Based on https://pdfs.semanticscholar.org/6d65/7e8bc933cea878538f4c36d309f716f0fdf6.pdf
    val source = Source.fromFile("day_10.in")
    val map = source.getLines.toList
    source.close()
    var max = 0
    var maxPos = (-1, -1)
    for {
      fromY <- map.indices
      fromX <- map.indices
    } {
      val visibleNum = countVisible(map, fromX, fromY)
      if (visibleNum > max) {
        max = visibleNum
        maxPos = (fromY, fromX)
      }
    }
    println(max, s"x: ${maxPos._2}", s"y: ${maxPos._1}")
  }
}
