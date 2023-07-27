package com.rockthejvm.strings

import scala.annotation.tailrec

object ReorganizeString extends App {

  // reorg a string such that the output contains no two like letter that are adjacent
  // any output that matches the conditions may be valid
  // ex. "aaabc" -> "abaca"
  // "aaa" -> ""

  // my attempt - doesn't work
//  def reorganizeString(str: String): String = {
//    // are all letters unique?
//    val strList = str.toList
//    val totalLength = strList.length
//    val uniqueLength = strList.toSet.size
//
//    // are any repeating characters next to one another
//    @tailrec
//    def checkRepeating(remaining: List[Char], current: Char): Boolean = {
//      if (remaining.isEmpty) false
//      else if (remaining.head == current) true
//      else checkRepeating(remaining.tail, remaining.head)
//    }
//
//    def reorgTailrec(remaining: List[Char], current: Char, acc: List[Char]): List[Char] = {
//      if (remaining.isEmpty) acc
//      else if (remaining.head == current) {
//
//      }
//      else checkRepeating(remaining.tail, remaining.head)
//    }
//
//
//
//    if (totalLength == uniqueLength) str
//    else if (!checkRepeating(strList.tail, strList.head)) str
//    else "no"
//
//  }

  // Daniel's impl
  def reorganizeString_v2(str: String): String = {

    /*
    { a -> 3, b -> 1, c -> 1 }
    = ot({ a -> 3, b -> 1, c -> 1 }, '', "")
    = ot({ a -> 2, b -> 1, c -> 1 }, 'a', "a")
    = ot({ a -> 2, c -> 1 }, 'b', "ab")
    = ot({ a -> 1, c -> 1 }, 'a', "aba")
    = ot({ a -> 1}, 'c', "abac")
    = ot({}, 'a', "abaca")
    = "abaca"
     */
    @tailrec
    def organizeTailrec(characterCount: Map[Char, Int], lastChar: Char = '\u0000', acc: String = ""): String = {
      if (characterCount.isEmpty) acc
      else {
        val newChar = characterCount.filter(_._1 != lastChar).maxBy(_._2)._1
        val newCharCount =
          if (characterCount(newChar) == 1) characterCount - newChar
          else characterCount + (newChar -> (characterCount(newChar) - 1))

        organizeTailrec(newCharCount, newChar, acc + newChar)
      }
    }


    //val charCountQuicker: Map[Char, Int] = str.groupBy(c => c).view.mapValues(_.length).toMap
    val charCount = str.foldLeft(Map[Char,Int]()) {
      case (map, char) => map + (char -> (map.getOrElse(char, 0) + 1))
    }

    if (charCount.values.exists(_ > (str.length + 1) / 2)) "" // impossible to reorg
    else organizeTailrec(charCount)
  }

  println(reorganizeString_v2("aaabc"))
  println(reorganizeString_v2("aaa"))
}
