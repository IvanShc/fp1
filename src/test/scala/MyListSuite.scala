import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import MyList._
import MyList.*

class MyListSuite extends ScalaCheckSuite {

  given [A: Arbitrary]: Arbitrary[MyList[A]] =
    Arbitrary(Gen.listOf(arbitrary[A]).map(MyList.of(_*)))

  property("Reverse Int element in MyList") {
    forAll { (xs: MyList[Int]) =>
      reverse(reverse(xs)) == xs
    }
  }

  property("Reverse String element in MyList") {
    forAll { (xs: MyList[String]) =>
      reverse(reverse(xs)) == xs
    }
  }


  property("First Int element in MyList") {
    forAll { (xs: MyList[Int]) =>
      first(xs) == last(reverse(xs))
    }
  }

  property("Last Int element in MyList") {
    forAll { (xs: MyList[Int]) =>
      last(xs) == first(reverse(xs))
    }
  }

  property("Count Int element in MyList") {
    forAll { (xs: MyList[Int], n:Int) =>
      (count(xs, n) >= 0) && (count(xs, n) <= Len(xs))
    }
  }



}
