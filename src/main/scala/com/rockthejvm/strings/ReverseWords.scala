package com.rockthejvm.strings

object ReverseWords extends App {

  /*
  "Alice loves Scala" => "Scala loves Alice"
  "hello world" => "world hello"

  trim leading and trailing whitespace, have one space between words
   */
  def reverseWords(string: String): String =
    string.split(" ").toList.filterNot(_.isEmpty).reverse.mkString(" ")

  println(reverseWords("Alice loves Scala"))
  println(reverseWords("     Alice     loves     Scala     "))

}
