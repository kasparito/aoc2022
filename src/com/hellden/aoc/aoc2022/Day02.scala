package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day02 extends Day(2):

  override def part1: Int =
    val value = IndexedSeq(
      "B X",
      "C Y",
      "A Z",
      "A X",
      "B Y",
      "C Z",
      "C X",
      "A Y",
      "B Z",
    )
    inputLines.map(value.indexOf(_) + 1).sum

  override def part2: Int =
    val value = IndexedSeq(
      "B X",
      "C X",
      "A X",
      "A Y",
      "B Y",
      "C Y",
      "C Z",
      "A Z",
      "B Z",
    )
    inputLines.map(value.indexOf(_) + 1).sum
