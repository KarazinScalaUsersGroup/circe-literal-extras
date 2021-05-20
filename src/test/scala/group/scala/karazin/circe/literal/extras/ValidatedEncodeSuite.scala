package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import io.circe.syntax._
import io.circe.{Json, parser}
import io.circe.disjunctionCodecs.encodeValidated
import org.scalacheck._
import org.scalacheck.Prop._

class ValidatedEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined primitive with Currency value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Validated[Int, String]]('sc, 'args)}

    forAll { (value: Validated[Int, String]) =>

//      val expected: Json = encode"$value"
      
      val result: Json = value.asJson

      val t: Validated[Int, String] = Invalid(0)
      
      print(t.asJson)

//      assertEquals(result, expected)
    }

  }
