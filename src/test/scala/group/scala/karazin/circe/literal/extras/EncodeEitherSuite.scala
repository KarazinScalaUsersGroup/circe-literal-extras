package group.scala.karazin.circe.literal.extras

import group.scala.karazin.circe.literal.extras.macros
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._
import io.circe.disjunctionCodecs._

class EncodeEitherSuite extends munit.ScalaCheckSuite:

  property("bug 1") {
    case class Bar(value: Int) derives Codec.AsObject
    given Arbitrary[Bar] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Bar(v)})

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Bar]('sc, 'args) }

    forAll { (value: Bar) =>

      val result: Json =
        encode""" {
                    "value": 0,
                    "fg": 0
                  }
                  """


      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  property("bug 2") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }

    forAll { (value: Either[Int, String]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined Either[Int, String] parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: Either[Int, String]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined Either[Option[Int], List[String]] parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }

    forAll { (value: Either[Int, List[String]]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }
