package com.rockthejvm.strings

import scala.annotation.tailrec

object RansomNote extends App {

  // returns true if the note can be constructed out of the magazine
  // can only use each letter from magazine once (don't duplicate)
  def ransomNote(note: String, magazine: String): Boolean = {

    // Daniel's impl but works the same as mine
    def buildMap(str: String): Map[Char, Int] = {
      str.foldLeft(Map[Char,Int]()){ case(acc, letter) =>
        acc + (letter -> (acc.getOrElse(letter, 0) + 1))
      }
    }

    @tailrec
    def countCharactersTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) acc
      else {
        val currentChar = remaining.head
        val currentCount = acc.getOrElse(currentChar, 0)
        countCharactersTailrec(remaining.tail, acc + (currentChar -> (currentCount + 1)))
      }
    }

    val noteCountMap = countCharactersTailrec(note, Map())
    val magazineCountMap = countCharactersTailrec(magazine, Map())

    noteCountMap.forall { case (letter, count) =>
      if (magazineCountMap.contains(letter)) magazineCountMap(letter) >= count
      else false
    }
  }

  println(ransomNote("some small message", "this is another message that has some of the same small words in it, so it should work"))
}
