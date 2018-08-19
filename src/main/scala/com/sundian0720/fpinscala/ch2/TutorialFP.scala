package com.sundian0720.fpinscala.ch2

object TutorialFP {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else
        go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def next(n: Int, initial1: Int, initial2: Int): Int = {
      if (n <= 0) initial1
      else if (n == 1) initial2
      else
        next(n - 1, initial2, initial1 + initial2)
    }

    next(n, 0, 1)
  }

  def isFindFirst[A](as: Array[A], f: A => Boolean): Int = {
    @annotation.tailrec
    def next(n: Int): Int = {
      if (n > as.length - 1) -1
      else if (f(as(n))) n
      else
        next(n + 1)
    }

    next(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def next(n: Int): Boolean = {
      if (n > as.length - 2) true
      else if (!ordered(as(n), as(n + 1))) false
      else
        next(n + 1)
    }

    next(0)
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.isEmpty) true
    else
      as.tail.foldLeft((as.head, true)) {
        case ((prev, res), curr) => (curr, res && ordered(prev, curr))
      }._2
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  def main(args: Array[String]): Unit = {
    val res = fib(5)
    println(s"fib n = 5 : $res")
    val res1 = isSorted(Array[Int](1, 3, 2, 8), (a1: Int, a2: Int) => if (a1 <= a2) true else false)
    println(s"[1,3,2,8] is sorted : $res1")
    val res2 = isSorted(Array[Int](1, 2, 3, 8), (a1: Int, a2: Int) => if (a1 <= a2) true else false)
    println(s"[1,2,3,8] is sorted : $res2")
    val res3 = isSorted2(Array[Int](1, 3, 2, 8), (a1: Int, a2: Int) => if (a1 <= a2) true else false)
    println(s"[1,3,2,8] is sorted : $res3")
    val res4 = isSorted2(Array[Int](1, 2, 3, 8), (a1: Int, a2: Int) => if (a1 <= a2) true else false)
    println(s"[1,2,3,8] is sorted : $res4")
  }

}
