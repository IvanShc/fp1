package com.tkroman.kpi.y2022.l1
import scala.None
import scala.annotation.tailrec
import scala.collection.immutable

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

def reverse[A](xs: List[A]): List[A] = {
  @tailrec
  def go[A](xs: List[A], result: List[A]): List[A] = {
    xs match
      case List.Nil => result
      case List.Cons(hd, tl) => go(tl, List.Cons(hd, result))
  }

  go(xs, List.Nil)
}

def count[A](xs: List[A], a: A): Int = {
  @tailrec
  def go(xs: List[A], a: A, result: Int): Int = {
    xs match
      case List.Nil => result
      case List.Cons(hd, tl) => go(tl, a, if (hd == a) result + 1 else result)
  }

  go(xs, a, 0)
}


def first[A](xs: List[A]): Option[A] = {
  xs match
    case List.Nil => None
    case List.Cons(hd, tl) => Some(hd)
}


@tailrec
def last[A](xs: List[A]): Option[A] = {
  xs match
    case List.Nil => None
    case List.Cons(hd, List.Nil) => Some(hd)
    case List.Cons(hd, tl) => last(tl)
}

@tailrec
def corresponds[A, B](xs: List[A], ys: List[B], f: (A, B) => Boolean): Boolean = {
  (xs, ys) match
    case (List.Nil, List.Nil) => true
    case (List.Cons(hd1, tl1), List.Cons(hd2, tl2)) =>
      if (f(hd1, hd2))
        corresponds(tl1, tl2, f)
      else
        false
    case default => false
}

object List:
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(List.Nil: List[A]) { case (x, acc) => List.Cons(x, acc) }

@main def run(): Unit =
  println("HI")