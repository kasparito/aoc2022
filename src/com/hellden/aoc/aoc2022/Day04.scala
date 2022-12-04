package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day04 extends Day(4):

  private val Pair = """(\d+)-(\d+),(\d+)-(\d+)""".r

  private def parsePair: String => (Set[Int], Set[Int]) =
    case Pair(a, b, c, d) =>
      val e1 = a.toInt to b.toInt
      val e2 = c.toInt to d.toInt
      (e1.toSet, e2.toSet)

  private val pairs = inputLines.map(parsePair)

  override def part1: Int = // 509
    pairs.count { (e1, e2) => e1.subsetOf(e2) || e2.subsetOf(e1) }

  override def part2: Int = // 870
    pairs.count { (e1, e2) => e1.intersect(e2).nonEmpty }
