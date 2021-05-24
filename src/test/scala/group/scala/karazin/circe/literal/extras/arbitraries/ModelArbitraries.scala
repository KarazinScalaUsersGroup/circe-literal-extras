package group.scala.karazin.circe.literal.extras.arbitraries

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import io.circe.{Json, JsonObject}

import group.scala.karazin.circe.literal.extras.model._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

trait ModelArbitraries {

  val genStr: Gen[String] = Gen.alphaStr
  
  val genBar: Gen[Bar] = for {
    str   <- Arbitrary.arbitrary[String]
    bool  <- Arbitrary.arbitrary[Boolean]
  } yield Bar(str, bool, ())

  val genBarLike: Gen[BarLike] = for {
    str   <- Arbitrary.arbitrary[String]
    bool  <- Arbitrary.arbitrary[Boolean]
  } yield BarLike(str, bool, ())

  val genBuzz: Gen[Buzz] = for {
    int   <- Arbitrary.arbitrary[Int]
    bool  <- Arbitrary.arbitrary[Boolean]
    short  <- Arbitrary.arbitrary[Short]
  } yield Buzz(int, bool, short)

  val genBuzzLike: Gen[BuzzLike] = for {
    int   <- Arbitrary.arbitrary[Int]
    bool  <- Arbitrary.arbitrary[Boolean]
    short  <- Arbitrary.arbitrary[Short]
  } yield BuzzLike(int, bool, short)

  val genFoo: Gen[Foo] = for {
    int  <- Arbitrary.arbitrary[Int]
    bar  <- Arbitrary.arbitrary[Option[Bar]]
    buzz <- Arbitrary.arbitrary[List[Buzz]]
    qux  <- Arbitrary.arbitrary[JsonObject]
  } yield Foo(int, bar, buzz, qux)

  val genFooLike: Gen[FooLike] = for {
    int  <- Arbitrary.arbitrary[Int]
    bar  <- Arbitrary.arbitrary[Bar]
    buzz <- Arbitrary.arbitrary[List[Buzz]]
    qux  <- Arbitrary.arbitrary[JsonObject]
  } yield FooLike(int, bar, buzz, qux)

  given Arbitrary[String] = Arbitrary(genStr)
  given Arbitrary[Buzz] = Arbitrary(genBuzz)
  given Arbitrary[BuzzLike] = Arbitrary(genBuzzLike)
  given Arbitrary[Bar] = Arbitrary(genBar)
  given Arbitrary[BarLike] = Arbitrary(genBarLike)
  given Arbitrary[Foo] = Arbitrary(genFoo)
  given Arbitrary[FooLike] = Arbitrary(genFooLike)

}
