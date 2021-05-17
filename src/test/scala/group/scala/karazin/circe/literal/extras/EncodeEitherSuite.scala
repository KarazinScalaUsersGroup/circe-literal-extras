package group.scala.karazin.circe.literal.extras

import group.scala.karazin.circe.literal.extras.macros
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._
import io.circe.disjunctionCodecs._

class EncodeEitherSuite extends munit.ScalaCheckSuite:

  property("inlined Either[Int, Int]") {
    case class Container(value: Either[Int, String]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }

    forAll { (value: Either[Int, String]) =>

      val container = Container(value)

//      lazy val result: Json =
//        encode"""
//              {
//                "value": $value
//              }
//            """
//
//      val expected: Json =
//        parser.parse(
//          s"""
//          {
//            "value": ${value.fold(_.toString, _.toString)}
//          }
//         """
//        ).toOption.get
//
//      assertEquals(result, expected)
      val test: Either[String, Int] = Left("test")
      print(test.asJson)
    }

  }
