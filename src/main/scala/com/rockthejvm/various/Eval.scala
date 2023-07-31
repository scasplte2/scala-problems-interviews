package com.rockthejvm.various

import scala.annotation.tailrec

object Eval extends App {

  def eval(expr: String): Int = {
    val operators = Set("+", "-", "*", "/")

    def getOperators: List[String] = expr.split(" ").filter(operators).toList
    def getNumbers: List[Int] = expr.split(" ").filterNot(operators).map(_.toInt).toList

    def simpleOperation(op1: Int, op2: Int, operator: String) = operator match {
      case "+" => op1 + op2
      case "-" => op1 - op2
      case "*" => op1 * op2
      case "?" => op1 / op2
      case _ => throw new IllegalArgumentException(s"Invalid operator: $operator")
    }

    def priority(operator: String): Int = operator match {
      case "+" | "-" => 1
      case "*" | "?" => 2
      case _ => 0
    }

    /*
    1 + 2 * 3

    etr([1,2,3], [+,*], [],[])
    etr([2,3], [+,*], [1], [])
    etr([2,3], [*], [1], [+])
    etr([3], [*], [2,1], [+])
    etr([3], [], [2,1], [*,+])
    etr([], [], [3,2,1], [*,+])
    etr([], [], [6,1], [+])
    etr([], [], [7], [])
    7
    -------
    1 * 2 + 3

    etr([1,2,3], [*,+], [], [])
    etr([2,3], [*,+], [1], [])
    etr([2,3], [+], [1], [*])
    etr([3], [+], [2,1], [*])
    etr([3], [+], [2], [])
    etr([3], [], [2], [+])
    etr([], [], [3,2], [+]
    etr([], [], [5], [])
    5
     */
    @tailrec
    def evalTailrec(
        remainingOperands: List[Int],
        remainingOperators: List[String],
        operandStack: List[Int],
        operatorStack: List[String]
     ): Int = {
      if (remainingOperands.isEmpty) {
        if (operatorStack.isEmpty) operandStack.head // final result
        else {
          // compute a simple operation and proceed
          val op2 = operandStack.head
          val op1 = operandStack.tail.head
          val operator = operatorStack.head
          val simpleResult = simpleOperation(op1, op2, operator)

          evalTailrec(remainingOperands, remainingOperators, simpleResult :: operandStack.tail.tail, operatorStack.tail)
        }
      } else if (remainingOperands.length > remainingOperators.length) {
        // pop an operand and proceed
        evalTailrec(remainingOperands.tail, remainingOperators, remainingOperands.head :: operandStack, operatorStack)
      } else if (operatorStack.isEmpty || priority(operatorStack.head) < priority(remainingOperators.head)) {
        // pop an oerator and proceed
        evalTailrec(remainingOperands, remainingOperators.tail, operandStack, remainingOperators.head :: operatorStack)
      } else {
        // compute the higher priority operation and proceed
        val op2 = operandStack.head
        val op1 = operandStack.tail.head
        val operator = operatorStack.head
        val simpleResult = simpleOperation(op1, op2, operator)

        evalTailrec(remainingOperands, remainingOperators, simpleResult :: operandStack.tail.tail, operatorStack.tail)
      }
    }

    evalTailrec(getNumbers, getOperators, List(), List())
  }

  println(eval("1 + 2 * 3")) // 7
  println(eval("1 * 2 + 3")) // 5
  println(eval("1 - 2 - 3 * 4")) // -13

}
