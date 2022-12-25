package com.hellden.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends Day(11):

  private abstract class Monkey(
    val n: Int,
    val items: mutable.Queue[Int],
    divisibleBy: Int,
    successMonkey: Int,
    failureMonkey: Int
  ):

    var inspections: Int = 0

    def operation(level: Int): Int

    def chooseMonkey(level: Int): Int =
      if level % divisibleBy == 0 then
        successMonkey
      else
        failureMonkey

  private val monkeys = IndexedSeq(
    new Monkey(0, mutable.Queue(79, 98), 23, 2, 3):
      def operation(level: Int): Int = level * 19
    ,
    new Monkey(1, mutable.Queue(54, 65, 75, 74), 19, 2, 0):
      def operation(level: Int): Int = level + 6
    ,
    new Monkey(2, mutable.Queue(79, 60, 97), 13, 1, 3):
      def operation(level: Int): Int = level * level
    ,
    new Monkey(3, mutable.Queue(74), 17, 0, 1):
      def operation(level: Int): Int = level + 3
  )

  override def part1: Int = // ???
    (1 to 20).foreach { _ =>
      monkeys.foreach { monkey =>
        monkey.items.dequeueAll(_ => true).foreach { level =>
          monkey.inspections += 1
          val newLevel = monkey.operation(level) / 3
          val newMonkey = monkey.chooseMonkey(newLevel)
          monkeys(newMonkey).items.enqueue(newLevel)
        }
      }
    }
    monkeys.map(_.inspections).sorted.reverse.take(2).product
