package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector, NonEmptyChain, Chain, Validated, OneAnd}
import cats.kernel.Order
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.util._

import scala.collection.immutable.{SortedMap, SortedSet}

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

  def genNonEmptyMap[K: Arbitrary, T: Arbitrary](using K: Order[K]): Gen[NonEmptyMap[K, T]] = for {
    key <- Arbitrary.arbitrary[K]
    value <- Arbitrary.arbitrary[T]
    map <- Arbitrary.arbitrary[Map[K, T]]
  } yield NonEmptyMap((key, value), SortedMap[K, T]() ++ map)

  def genChainFixedSize[T](using arbT: Arbitrary[T])(size: Int): Gen[Chain[T]] = {
    val fromSeq = Gen.listOfN(size, arbT.arbitrary).map(Chain.fromSeq)
    val recursive =
      size match {
        case 0 => Gen.const(Chain.nil)
        case 1 => arbT.arbitrary.map(Chain.one)
        case n =>
          // Here we concat two chains
          for {
            n0 <- Gen.choose(1, n - 1)
            n1 = n - n0
            left <- genChainFixedSize(n0)
            right <- genChainFixedSize(n1)
          } yield left ++ right
      }

    // prefer to generate recursively built Chains
    // but sometimes create fromSeq
    Gen.frequency((5, recursive), (1, fromSeq))
  }

  def genChain[T: Arbitrary]: Gen[Chain[T]] = for {
    size <- Gen.choose(0, 10)
    chain <- genChainFixedSize(size)
  } yield chain

  implicit def catsLawsArbitraryForNonEmptyChain[T: Arbitrary]: Arbitrary[NonEmptyChain[T]] =
    Arbitrary(implicitly[Arbitrary[Chain[T]]].arbitrary.flatMap { chain =>
      NonEmptyChain.fromChain(chain) match {
        case None     => Arbitrary.arbitrary[T].map(NonEmptyChain.one)
        case Some(ne) => Gen.const(ne)
      }
    })

  implicit def catsLawsArbitraryForValidated[A: Arbitrary, B: Arbitrary]: Arbitrary[Validated[A, B]] =
      Arbitrary(Gen.oneOf(
        Arbitrary.arbitrary[A].map(Validated.invalid),
        Arbitrary.arbitrary[B].map(Validated.valid)
      ))

  implicit def catsLawsArbitraryForOneAnd[F[_], A](implicit arbA: Arbitrary[A], arbF: Arbitrary[F[A]]): Arbitrary[OneAnd[F, A]] =
    Arbitrary(
      arbF.arbitrary flatMap { fa =>
        arbA.arbitrary.map(a => OneAnd(a, fa))
      }
    )

  given [T: Arbitrary]: Arbitrary[NonEmptyList[T]] = Arbitrary(genNonEmptyList[T])
  given [T: Arbitrary]: Arbitrary[NonEmptyVector[T]] = Arbitrary(genNonEmptyVector[T])
  given [T: Arbitrary](using ord: Ordering[T], order: Order[T]): Arbitrary[NonEmptySet[T]] = Arbitrary(genNonEmptySet[T])
  given [K: Arbitrary, T: Arbitrary](using order: Order[K]): Arbitrary[NonEmptyMap[K, T]] = Arbitrary(genNonEmptyMap[K, T])
  given [T: Arbitrary]: Arbitrary[Chain[T]] = Arbitrary(genChain[T])
}
