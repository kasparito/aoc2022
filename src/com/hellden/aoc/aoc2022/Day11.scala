package com.hellden.aoc.aoc2022

import me.tongfei.progressbar.{ProgressBar, ProgressBarBuilder, ProgressBarStyle}

import java.time.Duration
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends Day(11):

  private class Monkey(
    val items: mutable.Queue[BigInt],
    val operation: BigInt => BigInt,
    val divisibleBy: Int,
    successMonkey: Int,
    failureMonkey: Int
  ):

    var inspections: BigInt = 0

    def chooseMonkey(level: BigInt): Int =
      if level % divisibleBy == 0 then
        successMonkey
      else
        failureMonkey

  private def initMonkeys = IndexedSeq(
    new Monkey(
      mutable.Queue(54, 98, 50, 94, 69, 62, 53, 85),
      _ * 13,
      3,
      2,
      1
    ),
    new Monkey(
      mutable.Queue(71, 55, 82),
      _ + 2,
      13,
      7,
      2
    ),
    new Monkey(
      mutable.Queue(77, 73, 86, 72, 87),
      _ + 8,
      19,
      4,
      7
    ),
    new Monkey(
      mutable.Queue(97, 91),
      _ + 1,
      17,
      6,
      5
    ),
    new Monkey(
      mutable.Queue(78, 97, 51, 85, 66, 63, 62),
      _ * 17,
      5,
      6,
      3
    ),
    new Monkey(
      mutable.Queue(88),
      _ + 3,
      7,
      1,
      0
    ),
    new Monkey(
      mutable.Queue(87, 57, 63, 86, 87, 53),
      _.pow(2),
      11,
      5,
      0
    ),
    new Monkey(
      mutable.Queue(73, 59, 82, 65),
      _ + 6,
      2,
      4,
      3
    )
  )

  override def part1: BigInt = // 112221
    val monkeys = initMonkeys
    (1 to 20).foreach { _ =>
      monkeys.foreach { monkey =>
        try while true do
          val level = monkey.items.dequeue()
          monkey.inspections += 1
          val newLevel = monkey.operation(level) / 3
          val newMonkey = monkey.chooseMonkey(newLevel)
          monkeys(newMonkey).items.enqueue(newLevel)
        catch
          case _: NoSuchElementException =>
      }
    }
    monkeys.map(_.inspections).sorted.reverse.take(2).product

  override def part2: BigInt = // 25272176808
    val monkeys = initMonkeys
    val modulo = BigInt(monkeys.map(_.divisibleBy).product).pow(2)
    val progress = new ProgressBarBuilder()
      .setTaskName("Rounds")
      .setInitialMax(10000)
      .setUpdateIntervalMillis(100)
      .showSpeed()
      .build()
    (1 to 10000).foreach { _ =>
      monkeys.foreach { monkey =>
        try while true do
          val level = monkey.items.dequeue()
          monkey.inspections += 1
          val newLevel = monkey.operation(level) % modulo
          val newMonkey = monkey.chooseMonkey(newLevel)
          monkeys(newMonkey).items.enqueue(newLevel)
        catch
          case _: NoSuchElementException =>
      }
      progress.step()
    }
    monkeys.map(_.inspections).sorted.reverse.take(2).product
