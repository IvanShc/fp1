import scala.None
import scala.annotation.tailrec
import scala.collection.immutable

enum MyList[+A]:
  case Nil
  case Cons(hd: A, tl: MyList[A])


def Len[A](xs: MyList[A]): Int =  {  // Helper function
  xs match
  case MyList.Nil => 0
  case MyList.Cons(hd,tl) => 1 + Len(tl)

}



def reverse[A](xs: MyList[A]): MyList[A] = {
  @tailrec
  def go[A](xs: MyList[A], result: MyList[A]): MyList[A] = {
    xs match
      case MyList.Nil => result
      case MyList.Cons(hd, tl) => go(tl, MyList.Cons(hd, result))
  }

  go(xs, MyList.Nil)
}

def count[A](xs: MyList[A], a: A): Int = {
  @tailrec
  def go(xs: MyList[A], a: A, result: Int): Int = {
    xs match
      case MyList.Nil => result
      case MyList.Cons(hd, tl) => go(tl, a, if (hd == a) result + 1 else result)
  }

  go(xs, a, 0)
}


def first[A](xs: MyList[A]): Option[A] = {
  xs match
    case MyList.Nil => None
    case MyList.Cons(hd, tl) => Some(hd)
}


@tailrec
def last[A](xs: MyList[A]): Option[A] = {
  xs match
    case MyList.Nil => None
    case MyList.Cons(hd, MyList.Nil) => Some(hd)
    case MyList.Cons(hd, tl) => last(tl)
}

@tailrec
def corresponds[A, B](xs: MyList[A], ys: MyList[B], f: (A, B) => Boolean): Boolean = {
  (xs, ys) match
    case (MyList.Nil, MyList.Nil) => true
    case (MyList.Cons(hd1, tl1), MyList.Cons(hd2, tl2)) =>
      if (f(hd1, hd2))
        corresponds(tl1, tl2, f)
      else
        false
    case default => false
}

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyList.Nil: MyList[A]) { case (x, acc) => MyList.Cons(x, acc) }

@main def run(): Unit =
  println("HI")
  //print(corresponds(MyList(0,5), MyList(1,2), (x, y) => corresponds(MyList(1,2),MyList(0,5), (x, y) => x * 3 == y)))
