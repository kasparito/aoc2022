package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day03 extends Day(3):

  private def priority(item: Char): Int =
    if item.isLower then
      item - 'a' + 1
    else
      item - 'A' + 27

  private def priority(items: String): Int =
    val (compartment1, compartment2) = items.splitAt(items.length / 2)
    val sharedItems = compartment1.toSet.intersect(compartment2.toSet)
    priority(sharedItems.head)

  private def priority(group: Seq[String]): Int = {
    val sharedItems = group.foldLeft(('A' to 'z').toSet) {
      (remainingItems, items) =>
        remainingItems.intersect(items.toSet)
    }
    priority(sharedItems.head)
  }

  override def part1: Int = // 7850
    inputLines.map(priority).sum

  override def part2: Int = // 2581
    inputLines.grouped(3).map(priority).sum
