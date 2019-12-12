import scalaz.Scalaz._

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day10Part2 {
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def isVisible(map: List[String], fromX: Int, fromY: Int, toX: Int, toY: Int): Boolean = {
    val pos = (toY - fromY, toX - fromX)
    val divider = gcd(Math.abs(pos._1), Math.abs(pos._2))
    val vector = (pos._2 / divider, pos._1 / divider)
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
      toX <- map(toY).indices
      if (toX != fromX || toY != fromY) && map(toY)(toX) == '#'
    } {
      if (isVisible(map, fromX, fromY, toX, toY)) {
        counter += 1
      }
    }
    counter
  }

  def collectVisible(map: List[String], pos: (Int, Int)): ListBuffer[(Int, Int)] = {
    var result = new ListBuffer[(Int, Int)]()
    for {
      toY <- map.indices
      toX <- map(toY).indices
      if (toX != pos._2 || toY != pos._1) && map(toY)(toX) == '#' && isVisible(map, pos._2, pos._1, toX, toY)
    } {
      result.+=((toY, toX))
    }
    result
  }

  def angle(pos: (Int, Int)): Double = {
    val angle = Math.atan2(pos._1, -pos._2) * 180 / Math.PI
    if (angle < 0) return 360 + angle
    angle
  }


  def main(args: Array[String]) {
    // Based on https://pdfs.semanticscholar.org/6d65/7e8bc933cea878538f4c36d309f716f0fdf6.pdf
    val source = Source.fromFile("day_10.in")
    val map = source.getLines.toList
    source.close()
    var max = 0
    var stationPos = (-1, -1)
    for {
      fromY <- map.indices
      fromX <- map(fromY).indices
    } {
      val visibleNum = countVisible(map, fromX, fromY)
      if (visibleNum > max) {
        stationPos = (fromY, fromX)
        max = visibleNum
      }
    }
    println(s"station pos: $stationPos, max: $max")
    var visible = collectVisible(map, stationPos)
    visible = visible.sortWith((pos1, pos2) => angle((pos1._2 - stationPos._2, pos1._1 - stationPos._1)) < angle((pos2._2 - stationPos._2, pos2._1 - stationPos._1)))
    println(visible(199)._2 * 100 + visible(199)._1)
  }
}