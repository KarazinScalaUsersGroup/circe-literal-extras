package group.scala.karazin.circe.literal.extras

import cats.implicits._
import group.scala.karazin.circe.literal.extras.macros
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._

class EncodeListSuite extends munit.ScalaCheckSuite:
  
  property("inlined list of ints") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Int]]('sc, 'args) }
    
    forAll { (value: List[Int]) =>
      
      lazy val result: Json = encode"""$value"""

      val expected: Json = value.asJson
      
      assertEquals(result, expected)
    }

  }

  property("inlined nested lists parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[List[Int]]]('sc, 'args) }

    forAll { (value: List[List[Int]]) =>

      val result: Json = encode"""$value"""

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  property("inlined list of options") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]]]('sc, 'args) }

    forAll { (options: List[Option[Int]]) =>

      lazy val result: Json = encode"""$options"""

      val expected = options.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined container like object parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]]]('sc, 'args) }

    forAll { (value: Iterable[Int]) =>

      val result: Json = encode"""$value"""

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  property("inlined list of Primitive objects") {

    case class Primitive(value: Int) derives Codec.AsObject
    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Primitive(v)})

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Primitive]]('sc, 'args) }
    
    forAll { (value: List[Primitive]) =>

      val result: Json = encode"$value"

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  test("corrupted ints parsing compile error") {
    //TODO test doesn't work
    assertNoDiff(
      compileErrors(
        """
          import group.scala.karazin.circe.literal.extras.macros
          
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[List[Int]]('sc, 'args) }
           
          encode"[ -1, null ]"
        """
      ),
      """  """.stripMargin
    )
    
  }

  test("corrupted options parsing compile error") {
    //TODO test doesn't work
    assertNoDiff(
      compileErrors(
        """
          import group.scala.karazin.circe.literal.extras.macros
          
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[List[Option[Int]]]('sc, 'args) }
              
          encode"[ null, 42, { } ]" 
        """
      ),
      """  """.stripMargin
    )
    
  }
