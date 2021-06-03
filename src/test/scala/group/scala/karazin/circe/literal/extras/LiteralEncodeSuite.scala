package group.scala.karazin.circe.literal.extras

import cats.implicits._
import group.scala.karazin.circe.literal.extras.macros
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._

class LiteralEncodeSuite extends munit.ScalaCheckSuite:

  test("inlined 0 parsing") {

    case class Value(v: 0 | 1) derives Codec.AsObject
    
    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Value]('sc, 'args) }

    val value: Value = Value(1)
    
    lazy val result: Json = encode"""{ "v": 1 }"""

    val expected: Json = value.asJson

    assertEquals(result, expected)

  }