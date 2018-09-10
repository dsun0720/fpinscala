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

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match {
      case Left(v) => Left(v)
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => f(v)
      case _: Right[B] => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for (aa <- this;
           bb <- b
      )
        yield f(aa, bb)
    }


  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldLeft(Right(List.empty[A]): Either[E, List[A]]) {
    case (_, left: Left[E]) => left
    case (left: Left[E], _) => left
    case (Right(list), Right(v)) => Right(v :: list)
  }

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = {
    sequence(as.map(f))
  }


}
