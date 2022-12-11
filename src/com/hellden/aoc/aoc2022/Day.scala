package com.hellden.aoc.aoc2022

import scala.collection.Iterable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.io.Source

private abstract class Day(day: Int) {

  case class Position(x: Int, y: Int)

  enum Direction(val dx: Int, val dy: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)

    def move(position: Position): Position =
      Position(position.x + dx, position.y + dy)

  implicit class FutureWrapper[T](f: Future[T]) {
    def await: T = Await.result(f, Duration.Inf)
  }

  implicit class KeyValuePairs[K, V](pairs: Iterable[(K, V)]) {
    def groupKeyValue: Map[K, Iterable[V]] =
      pairs.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  implicit class PrintHelper[T](x: T) {
    def log(s: String): T = {
      println(s"$s: $x")
      x
    }
  }

  val input: String = {
    val source = Source.fromFile(f"input/day$day%02d.txt")
    val content =
      try {
        source.mkString
      } finally {
        source.close()
      }
    content
  }

  lazy val inputLines: Seq[String] = input.split('\n')

  def part1: Any = "TBD"

  def part2: Any = "TBD"

  def main(args: Array[String]): Unit = {
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
