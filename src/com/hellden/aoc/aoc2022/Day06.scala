package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day06 extends Day(6):

  private def indexAfterFirst(distinctChars: Int) =
    input
      .sliding(distinctChars)
      .zipWithIndex
      .collectFirst {
        case (packet, start)
          if packet.toSet.size == distinctChars =>
          start + distinctChars
      }
      .get

  override def part1: Int = // 1876
    indexAfterFirst(4)

  override def part2: Int = // 2202
    indexAfterFirst(14)
