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
      
      lazy val result: Json =
        encode"""
                ${value}
            """

      val expected: Json =
        parser.parse(
          s"""
              [ ${value.map(_.toString) mkString ", "} ]
             """
        ).toOption.get
      
      assertEquals(result, expected)
    }

  }

  property("inlined container object parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[List[Int]]]('sc, 'args) }

    forAll { (value: List[List[Int]]) =>

      val result: Json =
        encode"""
                $value
                """

      val expected =
        parser.parse(
          s"""
             [ 
                ${ value map {
                    list => s""" [ ${list.map(_.toString) mkString ", "} ] """
                  } mkString ", "
                } 
             ]
             """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  property("inlined list of options") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]]]('sc, 'args) }

    forAll { (options: List[Option[Int]]) =>

      lazy val result: Json =
        encode"""
                $options
                """

      val expected =
        parser.parse(
          s"""
              [ ${options.map(v => v.fold("null")(_.toString)) mkString ", "} ]
              """
        ).toOption.get

      assertEquals(result, expected)

    }
  }

  property("inlined container like object parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]]]('sc, 'args) }

    forAll { (value: Iterable[Int]) =>

      val result: Json =
        encode"""
                $value
                """

      val expected =
        parser.parse(
          s"""
             [ ${value.map(_.toString) mkString ", "} ]
             """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  property("inlined list of Bar objects") {

    case class Primitive(value: Int) derives Codec.AsObject
    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Primitive(v)})

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Primitive]]('sc, 'args) }
    
    forAll { (value: List[Primitive]) =>

      val result: Json =
        encode""" 
                 $value
                 """

      val expected =
        parser.parse(
          s"""
              [ 
                ${ value map {
                    primitive => s"""{ "value": ${primitive.value} }"""
                  } mkString ", "
                }
              ]
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  test("corrupted ints parsing compile error") {
    
    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Int]]('sc, 'args) }
        
    compileErrors(
      """
        encode\"\"\"
                 [ -1, 0.1 ]
              \"\"\"
      """
    )
  }

  test("corrupted options parsing compile error") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]]]('sc, 'args) }

    compileErrors(
      """
        encode\"\"\"
                  [ null, 42, "" ]
              \"\"\"
      """
    )
  }
