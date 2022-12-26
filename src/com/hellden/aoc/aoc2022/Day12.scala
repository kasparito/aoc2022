package com.hellden.aoc.aoc2022

import com.hellden.aoc.aoc2022.Day12.Map.Path

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.math.Ordered.orderingToOrdered

object Day12 extends Day(12):

  object Map:
    val grid = new Grid(inputLines.map(_.toIndexedSeq).toIndexedSeq)

    private def elevation(position: Position): Char =
      grid.valueFor(position) match
        case 'S' => 'a'
        case 'E' => 'z'
        case c => c

    object Path:
      def apply(position: Position*): Path =
        Path(position.toList)

    case class Path(elements: List[Position]) extends Ordered[Path]:
      def length: Int = elements.length
      def head: Position = elements.head
      def contains(position: Position): Boolean = elements.contains(position)
      def add(position: Position): Path = copy(elements = position :: elements)
      override def hashCode(): Int = length.hashCode() + 31 * head.hashCode()
      override def equals(obj: Any): Boolean = obj match
        case path: Path => path.length == length && path.head == head
        case _ => false
      override def compare(path: Path): Int =
        (length, head.x, head.y) compare (path.length, path.head.x, path.head.y)

    @tailrec
    def findPaths(paths: SortedSet[Path], bestSolution: Option[Path] = None): Option[Path] =
      println(s"paths: ${paths.size}, current: ${paths.head.length}, best: ${bestSolution.map(_.length).getOrElse(0)}")
      val path = paths.head
      if bestSolution.exists(_.length <= path.length + 1) then
        bestSolution
      else
        val currentPosition = path.head
        val currentElevation = elevation(currentPosition)

        val nextPaths = Direction
          .values
          .flatMap(grid.move(currentPosition, _))
          .collect {
            case next if elevation(next) - currentElevation <= 1 && !path.contains(next) =>
              path.add(next)
          }

        val remaining = paths.drop(1)
        val solution = nextPaths.find(path => grid.valueFor(path.head) == 'E')
        findPaths(remaining ++ nextPaths, solution.orElse(bestSolution))

  override def part1: Int = // 504
    val start = SortedSet(Map.grid.find('S').map(Path(_)).toSeq: _*)
    Map.findPaths(start).get.length - 1

  override def part2: Int = // 500
    val start = SortedSet((Map.grid.find('S') ++ Map.grid.find('a')).map(Path(_)).toSeq: _*)
    Map.findPaths(start).get.length - 1
