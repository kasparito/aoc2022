package com.hellden.aoc.aoc2022

import com.hellden.aoc.aoc2022.Day12.Map.Path

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.math.Ordered.orderingToOrdered
import scala.util.parsing.combinator.*


object Day13 extends Day(13):
  import com.hellden.aoc.aoc2022.Day13.Packet.Element

  private case class Packet(elements: Seq[Element])

  private object Packet extends RegexParsers:
    type Element = Int | Packet

    def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def element: Parser[Element] = number | packet
    def packet: Parser[Packet] = "[" ~ repsep(element, ",") ~ "]" ^^ { case _ ~ elements ~ _ => Packet(elements) }

    def parse(s: String): Packet =
      parse(packet, s) match
        case Success(result, _) => result
        case Failure(message, _) => throw new IllegalArgumentException(s"Invalid input: $s, $message")

  private implicit object ElementOrdering extends Ordering[Element] {
    override def compare(e1: Element, e2: Element): Int = (e1, e2) match
      case (i1: Int, i2: Int) => i1 compare i2
      case (i1: Int, p2: Packet) => compare(Packet(Seq(i1)), p2)
      case (p1: Packet, i2: Int) => compare(p1, Packet(Seq(i2)))
      case (p1: Packet, p2: Packet) =>
        val i1 = p1.elements.iterator
        val i2 = p2.elements.iterator
        while (i1.hasNext && i2.hasNext) {
          val result = compare(i1.next(), i2.next())
          if (result != 0) return result
        }
        i1.hasNext compare i2.hasNext
  }

  override def part1: Int = // 5720
    inputLines.toList.grouped(3).zipWithIndex.map {
      case (packet1 :: packet2 :: _, index) if Packet.parse(packet1) <= Packet.parse(packet2) =>
        index + 1
      case _ =>
        0
    }.sum

  override def part2: Int = // 23504
    val dividerPackets = List(Packet.parse("[[2]]"), Packet.parse("[[6]]"))
    val sortedPackets = (dividerPackets ::: inputLines.toList.filterNot(_.isBlank).map(Packet.parse)).sorted
    dividerPackets.map(sortedPackets.indexOf(_) + 1).product
