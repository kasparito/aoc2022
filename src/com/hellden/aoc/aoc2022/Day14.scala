package com.hellden.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends Day(14):

  class Cave:

    private val cave: mutable.Map[Position, Char] = mutable.Map.empty

    protected def horizontalBounds: Range.Inclusive =
      Range.inclusive(cave.keys.map(_.x).min, cave.keys.map(_.x).max)
    protected def verticalBounds: Range.Inclusive =
      Range.inclusive(cave.keys.map(_.y).min, cave.keys.map(_.y).max)

    private def init(): Unit =
      def drawLine: (Position, Position) => Unit =
        case (Position(x, y1), Position(x2, y2)) if x == x2 =>
          Range
            .inclusive(math.min(y1, y2), math.max(y1, y2))
            .foreach(y => cave.addOne(Position(x, y) -> '#'))
        case (Position(x1, y), Position(x2, y2)) if y == y2 =>
          Range
            .inclusive(math.min(x1, x2), math.max(x1, x2))
            .foreach(x => cave.addOne(Position(x, y) -> '#'))
        case _ =>

      def drawPath(path: String): Unit = path
        .split(" -> ")
        .map(_.split(','))
        .map { case Array(x, y) => Position(x.toInt, y.toInt) }
        .sliding(2)
        .foreach { case Array(p1, p2) => drawLine(p1, p2) }

      inputLines.foreach(drawPath)

    protected def drop: Position => Boolean =
      case Position(_, y) if y == verticalBounds.max => true
      case position => fall(position).exists(drop)

    protected def fall(position: Position): Option[Position] =
      val down = Direction.Down.move(position)
      free(down)
        .orElse(free(Direction.Left.move(down)))
        .orElse(free(Direction.Right.move(down)))
        .orElse {
          cave.addOne(position -> 'o')
          None
        }

    protected def free(position: Position): Option[Position] =
      Option.when(!cave.contains(position))(position)

    def run(): Int =
      init()
      while !drop(Position(500, 0)) do ()
      cave.values.count(_ == 'o')

    override def toString: String =
      val hb = horizontalBounds
      val vb = verticalBounds
      Range.inclusive(vb.start, vb.end + 2).map { y =>
        String.valueOf(hb.map(x => cave.getOrElse(Position(x, y), '.')).toArray)
      }.mkString("\n", "\n", "\n")

  override def part1: Int = // 696
    val cave = new Cave
    cave.run()

  override def part2: Int = // 23610
    val cave = new Cave:
      private lazy val floor = verticalBounds.max + 2
      override protected def drop: Position => Boolean = position =>
        fall(position) match
          case None => true
          case Some(next) =>
            @tailrec
            def loop(p: Position): Unit =
              fall(p) match
                case None =>
                case Some(n) => loop(n)
            loop(next)
            false
      override protected def free(position: Position): Option[Position] =
        super.free(position).filter(_.y < floor)
    cave.run()
