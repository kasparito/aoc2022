package com.hellden.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day07 extends Day(7):

  private object Terminal:

    trait Node:
      def name: String
      def size: Long
    case class File(name: String, size: Long) extends Node
    case class Directory(
      name: String,
      parent: Option[Directory] = None,
      content: mutable.Map[String, Node] = mutable.Map.empty
    ) extends Node:
      def size: Long = content.values.map(_.size).sum
      def root: Directory =
        parent match
          case None => this
          case Some(parent) => parent.root

    private val FilePattern = """(\d+) (\S+)""".r
    private val DirectoryPattern = """dir (\S+)""".r
    private val CommandPattern = """\$ (\S+)(?: (\S+))?""".r

    @tailrec
    def run(lines: List[String], current: Directory): Unit =
      lines match
        case Nil =>
        case command :: remaining =>
          val newCurrent =
            command match
              case FilePattern(size, name) =>
                current.content.update(name, File(name, size.toLong))
                current
              case DirectoryPattern(name) =>
                current.content.update(name, Directory(name, Some(current)))
                current
              case CommandPattern("cd", "/") =>
                current.root
              case CommandPattern("cd", "..") =>
                current.parent.get
              case CommandPattern("cd", directory) =>
                current.content(directory).asInstanceOf[Directory]
              case CommandPattern("ls", _) =>
                current
          run(remaining, newCurrent)

  import Terminal._

  private val root = Directory("/")
  run(inputLines.toList, root)

  override def part1: Long = // 1513699
    def sum(node: Node): Long =
      node match
        case directory: Directory if directory.size <= 100000 =>
          directory.size + directory.content.values.map(sum).sum
        case directory: Directory =>
          directory.content.values.map(sum).sum
        case _ =>
          0
    sum(root)

  override def part2: Long = // 7991939
    val remainingSpace = 70000000 - root.size
    val neededSpace = 30000000 - remainingSpace
    def find(node: Node): Iterable[Directory] =
      node match
        case directory: Directory =>
          Option.when(directory.size > neededSpace)(directory) ++
            directory.content.values.flatMap(find)
        case _ =>
          Iterable.empty
    find(root).map(_.size).min
