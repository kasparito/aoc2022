package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day04 extends Day(4):

  private val Pair = """(\d+)-(\d+),(\d+)-(\d+)""".r

  override def part1: Int = // 509
    inputLines.count {
      case Pair(a, b, c, d) =>
        val e1 = (a.toInt to b.toInt).toSet
        val e2 = (c.toInt to d.toInt).toSet
        e1.subsetOf(e2) || e2.subsetOf(e1)
    }

  override def part2: Int = // 870
    inputLines.count {
      case Pair(a, b, c, d) =>
        val e1 = (a.toInt to b.toInt).toSet
        val e2 = (c.toInt to d.toInt).toSet
        e1.intersect(e2).nonEmpty
    }
