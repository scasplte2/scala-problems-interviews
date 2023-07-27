package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {
  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) acc
      else {
        val currentChar = remaining.charAt(0)
        val currentCount = acc.getOrElse(currentChar, 0)
        countCharactersTailrec(remaining.substring(1), acc + (currentChar -> (currentCount + 1)))
      }
    }

    countCharactersTailrec(s, Map())
  }

  def testCountCharacters(): Unit = {
    println(countCharacters("Scala"))
  }
  //testCountCharacters()

  def checkAnagrams(a: String, b: String): Boolean = countCharacters(a) == countCharacters(b)
  def checkAnagrams_v2(a: String, b: String): Boolean = a.sorted == b.sorted

  def testCheckAnagrams(): Unit = {
    println("test v1")
    println(checkAnagrams("Scala", "Haskell"))
    println(checkAnagrams("desserts", "stressed"))

    println("test v2")
    println(checkAnagrams_v2("Scala", "Haskell"))
    println(checkAnagrams_v2("desserts", "stressed"))
  }
//  testCheckAnagrams()

  // my attempt
  def justify(text: String, width: Int): String = {
    val words = text.split(" ").toList

    @tailrec
    def justifyTailRec(remaining: List[String], acc: List[(String, Int)]): List[String] = {
      if (remaining.isEmpty) acc.map(_._1).reverse
      else {
        val currentLine = acc.head._1
        val currentLength = acc.head._2
        val nextWordLength = remaining.head.length
        if (currentLength + 1 + nextWordLength < width) {
          val newLine = s"$currentLine${remaining.head} "
          val newLength = currentLength + 1 + nextWordLength
          justifyTailRec(remaining.tail, (newLine, newLength) :: acc.tail)
        } else {
          val newLine = s"\n${remaining.head} "
          val newLength = nextWordLength
          justifyTailRec(remaining.tail, (newLine, newLength) :: acc)
        }
      }
    }

    @tailrec
    def addSpace(line: String): String = {
      if (line.length == width) line
      else addSpace(line + " ")
    }

    justifyTailRec(words, List(("", 0))).map{ line =>
        if (line.length < width) addSpace(line)
        else line
    }.mkString
  }

  def justify_v2(text: String, width: Int): String = {
    def createSpaces(n: Int): String = (1 to n).map(_ => " ").mkString("")

    @tailrec
    def pack(words: List[String], currentRow: List[String], count: Int, result: List[List[String]]): List[List[String]] = {
      if (words.isEmpty && currentRow.isEmpty) {
        // nothing else to add
        result
      } else if (words.isEmpty) {
        // add the last row
        result :+ currentRow
      } else if (currentRow.isEmpty && words.head.length > width) {
        // split the word across the lines
        val (wordOnThisRow, partOnNextRow) = words.head.splitAt(width - 2) // at width - 1 will be dash
        pack(partOnNextRow :: words.tail, List(), 0, result :+ List(wordOnThisRow + "-"))
      } else if (words.head.length + count > width) {
        // fetch a new row
        pack(words, List(), 0, result :+ currentRow)
      } else {
        // put the word into the current row
        pack(words.tail, currentRow :+ words.head, count + 1 + words.head.length, result)
      }
    }

    def justifyRow(row: List[String]): String = {
      if (row.length == 1) row.head
      else {
        val nSpacesAvailable = width - row.map(_.length).sum
        val nIntervals = row.length - 1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces = nSpacesAvailable % nIntervals
        val regularSpace = createSpaces(nSpacesPerInterval)
        val biggerSpace = createSpaces(nSpacesPerInterval + 1)

        if (nExtraSpaces == 0) row.mkString(regularSpace)
        else {
          val nWordsWithBiggerIntervals = nExtraSpaces + 1
          val wordsWithBiggerIntervals = row.take(nWordsWithBiggerIntervals)
          val firstPart = wordsWithBiggerIntervals.mkString(biggerSpace)
          val secondPart = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + regularSpace + secondPart
        }
      }
    }


    assert(width > 2)
    // split text into words
    val words = text.split(" ").toList
    // pack the words into rows
    val unjustifiedRows = pack(words, List(), 0, List())
    // justify the rows
    val justifiedRows = unjustifiedRows.init.map(justifyRow) :+ unjustifiedRows.last.mkString(" ")
    // rebuild the justified text
    justifiedRows.mkString("\n")
  }

  println(justify("This is some text that could go across multiple lines. I don't know that it will, but I am testing it.", 15))
  println(justify_v2("This is some text that could go across multiple lines. I don't know that it will, but I am testing it.", 15))
  println(justify_v2("Scala is the most amazing language you will ever write any code in", 5))
}
