package com.hellden.aoc.aoc2022

import scala.collection.mutable

object Day05 extends Day(5):

  private val StepPattern = """move (\d+) from (\d) to (\d)""".r

  private def initStacks(lines: Seq[String]): IndexedSeq[mutable.Stack[Char]] =
    val numberOfStacks = lines.map(l => (l.length + 1) / 4).max
    val stacks = IndexedSeq.fill(numberOfStacks)(mutable.Stack.empty[Char])
    lines.reverseIterator.foreach { line =>
      (0 until numberOfStacks).foreach { index =>
        val crate = line(index * 4 + 1)
        if crate.isLetter then
          stacks(index).push(crate)
      }
    }
    stacks

  private def run(maxStackHeight: Int, reverse: Boolean): String =
    val stacks = initStacks(inputLines.take(maxStackHeight))
    inputLines.drop(maxStackHeight + 2).foreach {
      case StepPattern(amount, from, to) =>
        val fromStack = stacks(from.toInt - 1)
        val toStack = stacks(to.toInt - 1)
        val crates = (0 until amount.toInt)
          .map(_ => fromStack.pop())
        (if reverse then crates.reverse else crates)
          .foreach(crate => toStack.push(crate))
    }
    stacks.map(_.pop()).mkString

  override def part1: String = // SHQWSRBDL
    run(maxStackHeight = 8, reverse = false)

  override def part2: String = // CDTQZHBRS
    run(maxStackHeight = 8, reverse = true)
