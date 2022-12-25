package com.hellden.aoc.aoc2022

import scala.annotation.tailrec

object Day10 extends Day(10):

  private val AddPattern = """addx (\S+)""".r

  private val execution = inputLines.foldLeft(IndexedSeq(1)) {
    case (history, "noop") =>
      history :+ history.last
    case (history, AddPattern(n)) =>
      history :++ IndexedSeq(history.last, history.last + n.toInt)
  }

  override def part1: Int = // 14040
    20.to(execution.size, 40).map { cycle =>
      val value = execution(cycle - 1)
      value * cycle
    }.sum

  override def part2: String = // ZGCJZJFL
    execution.take(240).grouped(40).map { row =>
      row.zipWithIndex.map {
        case (spritePos, pixel) if math.abs(pixel - spritePos) <= 1 =>
          "#"
        case _ =>
          "."
      }.mkString
    }.mkString("\n", "\n", "\n")
