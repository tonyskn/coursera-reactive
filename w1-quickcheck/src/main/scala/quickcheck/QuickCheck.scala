package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

//  property("min2") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    findMin(insert(m,h)) == m
//  }
//
//  property("inserting_2_els_into_empty_heap") = forAll { (a: A, b: A) =>
//    val h = insert(a, insert(b, empty))
//    val m = findMin(h)
//    m == ord.min(a, b)
//  }

//  property("inserting_then_deleting_minimum") = forAll { (a: A) =>
//    val h = insert(a, empty)
//    isEmpty(deleteMin(h))
//  }

//  property("minimum_of_melding") = forAll { (h1: H, h2: H) =>
//    val minMelded = findMin( meld(h1, h2) )
//    minMelded == findMin(h1) || minMelded == findMin(h2)
//  }

//  property("asList_should_be_sorted") = forAll { (h: H) =>
//    asList(h).sorted == asList(h) 
//  }

  property("inserting_sequence") = forAll { (l: List[A]) =>
    var h = l.foldRight(empty)(insert)
    asList(h) == l.sorted
  }

  def asList(h: H): List[A] = // the sorted list resulting from extracting all elements of h
   if (isEmpty(h)) List() else findMin(h) :: asList(deleteMin(h))


  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(empty, genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
