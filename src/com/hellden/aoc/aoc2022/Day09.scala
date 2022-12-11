package com.hellden.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day09 extends Day(9):

  private val MovePattern = """(\w) (\d+)""".r

  private def moves: String => Iterable[Direction] =
    case MovePattern("U", n) =>
      Iterable.fill(n.toInt)(Direction.Up)
    case MovePattern("D", n) =>
      Iterable.fill(n.toInt)(Direction.Down)
    case MovePattern("L", n) =>
      Iterable.fill(n.toInt)(Direction.Left)
    case MovePattern("R", n) =>
      Iterable.fill(n.toInt)(Direction.Right)

  private def stepTowards(from: Int, towards: Int): Int =
    if (from == towards)
      from
    else if (from < towards)
      from + 1
    else
      from - 1

  private def moveKnots(rope: List[Position]): List[Position] =
    rope match
      case first :: second :: tail =>
        val nextHead =
          if math.abs(first.x - second.x) <= 1 && math.abs(first.y - second.y) <= 1 then
            second
          else
            second.copy(
              x = stepTowards(second.x, first.x),
              y = stepTowards(second.y, first.y)
            )
        first :: moveKnots(nextHead :: tail)
      case _ =>
        rope

  private def moveRope(n: Int): Int =
    val size = 30
    val start = Position(size / 2, size / 2)
    val (_, visited) = inputLines.flatMap(moves).foldLeft((List.fill(n)(start), Set(start))) {
      case ((head :: tail, visited), direction) =>
        val newRope = moveKnots(direction.move(head) :: tail)
        (newRope, visited + newRope.last)
    }
    visited.size

  override def part1: Long = // 6464
    moveRope(2)

  override def part2: Long = // 2604
    moveRope(10)
