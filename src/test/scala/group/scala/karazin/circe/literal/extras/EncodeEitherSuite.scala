package group.scala.karazin.circe.literal.extras

import group.scala.karazin.circe.literal.extras.macros
import io.circe._
import io.circe.disjunctionCodecs._
import io.circe.syntax._
import org.scalacheck.Prop._
import org.scalacheck._

class EncodeEitherSuite extends munit.ScalaCheckSuite:

//  property("bug 1") {
//    case class Bar(value: Int) derives Codec.AsObject
//    given Arbitrary[Bar] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Bar(v)})
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Bar]('sc, 'args) }
//
//    forAll { (value: Bar) =>
//
//      val result: Json =
//        encode""" {
//                    "value": 0,
//                    "fg": 0
//                  }
//                  """
//
//
//      val expected: Json = value.asJson
//
//      assertEquals(result, expected)
//    }
//
//  }

  
  // TODO add this test as non-compiled
//  property("bug 2 fixed") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }
//
//    forAll { (value: Either[Int, String]) =>
//
//      val result: Json = encode""" $value """
//
//      val expected: Json = value.asJson
//
//      assertEquals(result, expected)
//    }
//
//  }

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

  property("inlined int parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: Int) =>
      
      val either: Either[Int, String] = Left(value)
      
      val result: Json =
        encode""" {
                    "Left": $value
                  }
                  """
      
      val expected: Json = either.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined string parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: String) =>

      val either: Either[Int, String] = Right(value)

      val result: Json =
        encode""" {
                    "Right": $value
                  }
                  """

      val expected: Json = either.asJson

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
