package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day01 extends Day(1):

  private val elvesCalories: List[Long] =
    val it = inputLines.iterator
    val builder = List.newBuilder[Long]
    while it.hasNext do
      builder.addOne(it
        .takeWhile(_.nonEmpty)
        .toList
        .map(_.toInt)
        .sum)
    builder.result()

  override def part1: Long =
    elvesCalories.max

  override def part2: Long =
    elvesCalories.sorted.takeRight(3).sum
