package group.scala.karazin.circe.literal.extras

import cats.implicits._
import group.scala.karazin.circe.literal.extras.macros
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._

class EncodeListSuite extends munit.ScalaCheckSuite:
  
  property("inlined list of strings") {
    case class Container(value: List[Int]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }
    
    forAll { (value: List[Int]) =>
      
      val container = Container(value)
      
      lazy val result: Json =
        encode"""
              {
                "value": ${value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": [ ${value.map(_.toString) mkString ", "} ]
          }
         """
        ).toOption.get
      
      assertEquals(result, expected)
    }

  }

  property("inlined list of options") {

    case class Container(value: List[Option[Int]]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }

    forAll { (options: List[Option[Int]]) =>

      lazy val result: Json =
        encode"""
                {
                  "value": $options
                }
                """

      val expected =
        parser.parse(
          s"""
              {
                "value": [ ${options.map(v => v.fold("null")(_.toString)) mkString ", "} ]
              }
              """
        ).toOption.get

      assertEquals(result, expected)

    }
  }

  property("inlined list of Bar objects") {

    case class Bar(value: Int) derives Codec.AsObject
    given Arbitrary[Bar] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Bar(v)})
    
    case class Container(value: List[Bar]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }
    
    forAll { (value: List[Bar]) =>

      val result: Json =
        encode""" 
                 {
                   "value": $value
                 }
                 """

      val expected =
        parser.parse(
          s"""
              {
                "value": [ ${value map {
                    bar => s"""{ "value": ${bar.value} }"""
                  } mkString ", "
                }]
              }
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  property("inlined container object parsing") {

    case class Container(value: List[List[Int]]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }
    
    forAll { (value: List[List[Int]]) =>

      val container = Container(value)
      
      val result: Json =
        encode"""
                $container
                """

      val expected =
        parser.parse(
          s"""
              {
                "value": [ ${value map {
                    list => s""" [ ${list.map(_.toString) mkString ", "} ] """
                  } mkString ", "
                } ]
              }
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  property("inlined container like object parsing") {

    case class Container(value: List[Option[Int]]) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Container]('sc, 'args) }

    forAll { (value: List[Int]) =>

      val result: Json =
        encode"""
                 {
                   "value": $value
                 }
                """

      val expected =
        parser.parse(
          s"""
              {
                "value": [ ${value.map(_.toString) mkString ", "} ]
              }
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  test("corrupted ints parsing compile error") {
    compileErrors(
      """
         case class Container(value: List[Int]) derives Codec.AsObject

        extension (inline sc: StringContext)
          inline def encode(inline args: Any*): Json =
            ${ macros.encode[Container]('sc, 'args) }
         
        encode\"\"\"
                {
                  "value": [ -1, "" ]
                }
        \"\"\"
      """
    )
  }

  test("corrupted options parsing compile error") {
    compileErrors(
      """
         case class Container(value: List[Option[Int]]) derives Codec.AsObject

        extension (inline sc: StringContext)
          inline def encode(inline args: Any*): Json =
            ${ macros.encode[Container]('sc, 'args) }
         
        encode\"\"\"
                {
                  "value": [ null, 42, "" ],
                }
        \"\"\"
      """
    )
  }
