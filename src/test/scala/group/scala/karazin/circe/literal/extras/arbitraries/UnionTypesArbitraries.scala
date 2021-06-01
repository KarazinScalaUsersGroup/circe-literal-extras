package group.scala.karazin.circe.literal.extras.arbitraries

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import io.circe.syntax._

import io.circe.Encoder

trait UnionTypesArbitraries {

  val intOrString: Gen[Int | String] =
    for {
      t   <- Gen.choose(0, 1)
      int <- Arbitrary.arbitrary[Int]
      str <- Arbitrary.arbitrary[String]
    } yield t match
        case 0 ⇒ int
        case 1 ⇒ str

  val intOrBoolean: Gen[Int | Boolean] =
    for {
      t   <- Gen.choose(0, 1)
      int <- Arbitrary.arbitrary[Int]
      bool <- Arbitrary.arbitrary[Boolean]
    } yield t match
      case 0 ⇒ int
      case 1 ⇒ bool

  given intOrStringArbitrary: Arbitrary[Int | String] = Arbitrary(intOrString)

  given intOrBooleanArbitrary: Arbitrary[Int | Boolean] = Arbitrary(intOrBoolean)

  given intOrStringEncoder: Encoder[Int | String] = value =>
    value match
      case v: Int ⇒ v.toInt.asJson
      case v: String ⇒ v.toString.asJson

  given intOrBooleanEncoder: Encoder[Int | Boolean] = value =>
    value match
      case v: Int ⇒ v.toInt.asJson
      case v: Boolean ⇒ v.asJson
}
