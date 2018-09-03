package com.sundian0720.fpinscala.ch3

import java.util.NoSuchElementException

object FunctionalDataStructures {

  sealed trait List[+A] {
    def head: A

    def tail: List[A]

    def isEmpty: Boolean

    //This method returns a List consisting of all but the last element of a List
    def init: List[A]

    def foreach(f: A => Unit): Unit = {
      @annotation.tailrec
      def iter(list: List[A]): Unit = {
        if (list.isEmpty) {}
        else {
          f(list.head)
          iter(list.tail)
        }
      }

      iter(this)
    }

    def drop(n: Int): List[A] = {
      @annotation.tailrec
      def iter(n: Int, list: List[A]): List[A] = {
        if (n <= 0) list
        else
          iter(n - 1, list.tail)
      }

      iter(n, this)
    }

    def dropWhile(f: A => Boolean): List[A] = {
      @annotation.tailrec
      def iter(list: List[A]): List[A] = {
        if (list.isEmpty || !f(list.head)) list
        else
          iter(list.tail)
      }

      iter(this)
    }

    def append2[B >: A](list: List[B]): List[B] = {
      if (isEmpty) list
      else
        Cons(head, tail.append(list))
    }

    def append[B >: A](list: List[B]): List[B] = {
      reverse.foldLeft(list)((b, a) => Cons(a, b))
    }

    def add[B >: A](element: B): List[B] = {
      append(Cons(element, NIL))
    }

    def ++[B >: A](list: List[B]): List[B] = append(list)

    def +[B >: A](element: B): List[B] = add(element)

    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      @annotation.tailrec
      def iter(list: List[A], b: B): B = {
        list match {
          case NIL => b
          case Cons(h, t) => iter(t, f(b, h))
        }
      }

      iter(this, z)
    }

    def reverse: List[A] = {
      foldLeft(NIL: List[A])((b, a) => Cons(a, b))
    }

    def foldRight[B](z: B)(f: (A, B) => B): B = {
      foldLeft(z)((b, a) => f(a, b))
    }

    def length: Int = {
      foldLeft(0) { case (b, _) => b + 1 }
    }

    def map[B](f: A => B): List[B] = {
      reverse.foldLeft(NIL: List[B])((b, a) => Cons(f(a), b))
    }

    def flatMap[B](f: A => List[B]): List[B] = {
      foldLeft(NIL: List[B])((b, a) => b ++ f(a))
    }

    def filter(f: A => Boolean): List[A] = {
      reverse.foldLeft(NIL: List[A])((b, a) => if (f(a)) Cons(a, b) else b)
    }

    def filter2(f: A => Boolean): List[A] = {
      flatMap(a => if (f(a)) List(a) else NIL.asInstanceOf[List[A]])
    }

    def zipWith[B, C](other: List[B])(f: (A, B) => C): List[C] = {
      @annotation.tailrec
      def iter(a: List[A], b: List[B], list: List[C]): List[C] = {
        (a, b) match {
          case (_, NIL) | (NIL, _) => list
          case (Cons(ha, ta), Cons(hb, tb)) => iter(ta, tb, list + f(ha, hb))

        }
      }

      iter(this, other, NIL.asInstanceOf[List[C]])
    }

    def hasSubsequence[A1 >: A](sub: List[A1]): Boolean = {
      sub.foldLeft(true) {
        case (false, _) => false
        case (true, a) => !this.filter(_ == a).isEmpty
      }
    }

    def reduce[A1 >: A](f: (A1, A1) => A1): A1 = {
      this match {
        case NIL => throw new UnsupportedOperationException("empty list")
        case Cons(h, t) => t.foldLeft(h.asInstanceOf[A1])(f)
      }
    }

  }


  case object NIL extends List[Nothing] {

    override def toString: String = "[]"

    override def head: Nothing = throw new NoSuchElementException("head of empty list")

    override def tail: List[Nothing] = throw new UnsupportedOperationException("tail of empty list")

    override def isEmpty: Boolean = true

    override def init: List[Nothing] = throw new UnsupportedOperationException("an empty list")
  }

  case class Cons[+A](override val head: A, override val tail: List[A]) extends List[A] {


    override def toString: String = {
      @annotation.tailrec
      def iter(list: List[A], str: String = ""): String = {
        list match {
          case Cons(h, NIL) => if (str.isEmpty) s"[$h]" else s"$str$h]"
          case Cons(h, t) if t != NIL =>
            val ss = if (str.isEmpty) s"[$h," else s"$str$h,"
            iter(t, ss)
        }
      }

      iter(this)
    }

    override def isEmpty: Boolean = false

    override def init: List[A] = {
      this match {
        case Cons(h, Cons(_, NIL)) => Cons(h, NIL)
        case Cons(h, t) => Cons(h, t.init)
      }
    }
  }

  object List {
    def apply[A](xs: A*): List[A] = if (xs.isEmpty) NIL else Cons(xs.head, apply(xs.tail: _*))

    def concatenate[A](list: List[List[A]]): List[A] = {
      list.foldLeft(NIL: List[A])((acc, ele) => acc.append(ele))
    }
  }

  sealed trait Tree[+A] {

    def value: A

    def left: Tree[A]

    def right: Tree[A]

    def depth: Int = {
      @annotation.tailrec
      def iter(list: List[Tree[A]], n: Int): Int = {
        list match {
          case NIL => n
          case _ =>
            val nl = list.foldLeft(NIL: List[Tree[A]]) {
              case (b, _: Leaf[A]) => b
              case (b, Branch(l, r)) => b ++ List(l, r)
            }
            iter(nl, n + 1)
        }
      }

      iter(List(this), 0)
    }

    def size: Int = {
      @annotation.tailrec
      def iter(list: List[Tree[A]], n: Int): Int = {
        list match {
          case NIL => n
          case Cons(_: Leaf[A], t) => iter(t, n + 1)
          case Cons(Branch(l, r), t) => iter(Cons(l, Cons(r, t)), n + 1)
        }
      }

      iter(List(this), 0)
    }

    def size2: Int = fold(a => 1)(_ + _ + 1)

    def map[B](f: A => B): Tree[B] = {
      this match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
      }
    }

    def maps[B](f: A => B): Tree[B] = {
      fold(a => Leaf(f(a)).asInstanceOf[Tree[B]])((l, r) => Branch(l, r))
    }

    def fold[B](f: A => B)(g: (B, B) => B): B = {
      def iter(trees: List[Tree[A]], list: List[B]): B = {
        trees match {
          case NIL => list.reverse.reduce(g)
          case Cons(Leaf(v), t) => iter(t, Cons(f(v), list))
          case Cons(Branch(l, r), t) => iter(Cons(l, Cons(r, t)), list)
        }
      }

      iter(List(this), NIL.asInstanceOf[List[B]])
    }

  }

  case class Leaf[+A](override val value: A) extends Tree[A] {

    override def left: Tree[A] = throw new NoSuchElementException("Leaf of a Tree")

    override def right: Tree[A] = throw new NoSuchElementException("Leaf of a Tree")
  }

  case class Branch[+A](override val left: Tree[A], override val right: Tree[A]) extends Tree[A] {
    override def value: A = throw new NoSuchElementException("Branch doesn't contain any value")
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 6)
    println(s"toString: $list")
    println(s"init: ${list.init}")
    println(s"[1,2] appends [3,6] = ${List(1, 2) ++ List(3, 6)}")
    println(s"[1,2,3] adds 6 = ${List(1, 2, 3) + 6}")
    println(s"foldLeft - sum [1,2,3,6] = ${list.foldLeft(0)(_ + _)}")
    println(s"foldRight - product [1,2,3,6] = ${list.foldRight(1)(_ * _)}")
    println(s"reverse [1,2,3,6] : ${list.reverse}")
    println(s"length of [1,2,3,6] = ${list.length}")
    println(s"concatenate [[1,2],[3,6]] : ${List.concatenate(List(List(1, 2), List(3, 6)))}")
    println(s"map of [1,2,3,6] : ${list.map(_ + 1)}")
    println(s"flatMap of [[1],[2,[3],[6]]] : ${list.flatMap(List(_))}")
    println(s"filter of [1,2,3,6] : ${list.filter(_ % 2 == 0)}")
    println(s"zip with [1,3] [2,6] : ${List(1, 3).zipWith(List(2, 6))((_, _))}")
    println(s"[1,3] is a sub sequence of [1,2,3,6] : ${list.hasSubsequence(List(1, 3))}")
    println(s"reduce - sum [1,2,3,6] = ${list.reduce(_ + _)}")
  }


}
