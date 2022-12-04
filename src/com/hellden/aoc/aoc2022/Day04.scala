package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day04 extends Day(4):

  private val Pair = """(\d+)-(\d+),(\d+)-(\d+)""".r

  private def parsePair: String => (Range, Range) =
    case Pair(a, b, c, d) =>
      (a.toInt to b.toInt, c.toInt to d.toInt)

  private val pairs = inputLines.map(parsePair)

  override def part1: Int = // 509
    pairs.count { (e1, e2) => e1.containsSlice(e2) || e2.containsSlice(e1) }

  override def part2: Int = // 870
    pairs.count { (e1, e2) => e1.intersect(e2).nonEmpty }
