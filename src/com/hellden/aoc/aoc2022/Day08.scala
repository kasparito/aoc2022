package com.hellden.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day08 extends Day(8):

  object Forest:
    val map = new Grid(inputLines.map(_.map(_.toString.toInt).toIndexedSeq).toIndexedSeq)

    def positions: Iterable[Position] =
      for
        x <- map.horizontalBounds
        y <- map.verticalBounds
      yield
        Position(x, y)

    def visible(position: Position): Boolean =
      val height = map.valueFor(position)

      @tailrec
      def visible(position: Position, direction: Direction): Boolean =
        map.move(position, direction) match
          case None =>
            true
          case Some(nextPosition) =>
            map.valueFor(nextPosition) < height && visible(nextPosition, direction)

      visible(position, Direction.Up) ||
        visible(position, Direction.Down) ||
        visible(position, Direction.Left) ||
        visible(position, Direction.Right)

    def scenicScore(position: Position): Long =
      val height = map.valueFor(position)

      @tailrec
      def score(position: Position, direction: Direction, acc: Long = 0): Long =
        map.move(position, direction) match
          case None =>
            acc
          case Some(nextPosition) =>
            if map.valueFor(nextPosition) >= height then
              acc + 1
            else
              score(nextPosition, direction, acc + 1)

      score(position, Direction.Up) *
        score(position, Direction.Down) *
        score(position, Direction.Left) *
        score(position, Direction.Right)

  override def part1: Long = // 1688
    Forest.positions.count(Forest.visible)

  override def part2: Long = // 410400
    Forest.positions.map(Forest.scenicScore).max
