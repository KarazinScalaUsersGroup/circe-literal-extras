package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.data.{NonEmptyList, NonEmptySet, NonEmptyVector}
import cats.kernel.Order
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.util._

import scala.collection.immutable.SortedSet

object arbitraries {
  def genNonEmptyList[T: Arbitrary]: Gen[NonEmptyList[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Arbitrary.arbitrary[List[T]]
  } yield NonEmptyList(head, tail)

  def genNonEmptyVector[T: Arbitrary]: Gen[NonEmptyVector[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Arbitrary.arbitrary[Vector[T]]
  } yield NonEmptyVector(head, tail)

  def genNonEmptySet[T: Arbitrary](using ord: Ordering[T], order: Order[T]): Gen[NonEmptySet[T]] = for {
    head <- Arbitrary.arbitrary[T]
    set <- Arbitrary.arbitrary[Set[T]]
  } yield NonEmptySet(head, SortedSet[T]() ++ set)

  given [T: Arbitrary]: Arbitrary[NonEmptyList[T]] = Arbitrary(genNonEmptyList[T])
  given [T: Arbitrary]: Arbitrary[NonEmptyVector[T]] = Arbitrary(genNonEmptyVector[T])
  given [T: Arbitrary](using ord: Ordering[T], order: Order[T]): Arbitrary[NonEmptySet[T]] = Arbitrary(genNonEmptySet[T])
}
