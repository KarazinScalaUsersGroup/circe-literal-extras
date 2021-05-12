package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.data.{NonEmptyList}
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.util._

object arbitraries {
  def genNonEmptyList[T: Arbitrary]: Gen[NonEmptyList[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Arbitrary.arbitrary[List[T]]
  } yield NonEmptyList(head, tail)

  given [T: Arbitrary]: Arbitrary[NonEmptyList[T]] = Arbitrary(genNonEmptyList[T])
}
