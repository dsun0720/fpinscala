package com.sundian0720.fpinscala.ch4

object FunctionalErrorHandling {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
      this match {
        case NONE => NONE
        case Some(a) => Some(f(a))
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case NONE => NONE
        case Some(a) => f(a)
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case NONE => default
        case Some(b) => b
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case NONE => ob
        case _: Some[B] => this
      }
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => Some(a)
      case _ => NONE
    }

    def lift[A1 >: A, B](f: A1 => B): Option[A1] => Option[B] = _ map f

    def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] =
      for (x <- this;
           y <- b
      ) f(x, y)

  }

  case class Some[+A](get: A) extends Option[A]

  case object NONE extends Option[Nothing]

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft(Some(List.empty[A]): Option[List[A]]) {
      case (_, NONE) => NONE
      case (NONE, _) => NONE
      case (Some(list), Some(v)) => Some(v :: list)
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a.map(f))
  }


}
