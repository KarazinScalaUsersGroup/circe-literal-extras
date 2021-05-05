package group.scala.karazin.circe.literal.extras.list

import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe._
import org.scalacheck.Prop._
import org.scalacheck._

import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.list.model.{given, _}

class EncodeListSuite extends munit.ScalaCheckSuite:
  
  extension (inline sc: StringContext)
    inline def encode(inline args: Any*): Json =
      ${ macros.encode[Foo]('sc, 'args) }

  test("raw json list parsing") {

    lazy val result: Json =
      encode"""
              {
                "ints": [ ],
                "options": [ null, true ],
                "bars": [ ]
              }
              """

      val expected =  
        parser.parse(  
          s"""
          {
            "ints": [ ],
            "options": [ null, true ],
            "bars": [ ]
          }
          """
        ).toOption.get                

      assertEquals(result, expected)

  }

  property("inlined ints, options, bars into foo object parsing") {

    forAll { (ints: List[Int], options: List[Option[Boolean]], bars: List[Bar]) =>

      val result =
        encode"""{
                    "ints": $ints,
                    "options": $options,
                    "bars": $bars
                 }
                """

      val expected =
        parser.parse(
          s"""
              {
                "ints": [ ${ints map(_.toString) mkString ", "} ],
                "options": [ ${options.map(v => v.fold("null")(_.toString)) mkString ", "} ],
                "bars": [ ${bars map {
            bar => s"""{ "str": "${bar.str}" }"""
          } mkString ", "
          }]
              }
              """
        ).toOption.get

      assertEquals(result, expected)

    }
  }

  property("inlined foo object parsing") {

    forAll { (foo: Foo) =>
      
      val result: Json =
        encode"""
                $foo
                """

      val expected =
        parser.parse(
          s"""
              {
                "ints": [ ${foo.ints map(_.toString) mkString ", "} ],
                "options": [ ${foo.options.map(v => v.fold("null")(_.toString)) mkString ", "} ],
                "bars": [ ${foo.bars map {
                    bar => s"""{ "str": "${bar.str}" }"""
                  } mkString ", "
                }]
              }
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  property("inlined foo-like object parsing") {

    forAll { (fooLike: FooLike) =>

      val result: Json =
        encode"""
                $fooLike
                """

      val expected =
        parser.parse(
          s"""
              {
                "ints": [ ${fooLike.ints map(_.toString) mkString ", "} ],
                "options": [ ${fooLike.options.map(_.toString) mkString ", "} ],
                "bars": [ ${fooLike.bars map {
                    bar => s""" { "str": "${bar.str}" } """
                  } mkString ", "
                }]
              }
              """
        ).toOption.get

      assertEquals(result, expected)
    }
  }

  test("corrupted ints parsing compile error") {
    compileErrors(
      """
        encode\"\"\"
                {
                  "ints": [ -1, "" ],
                  "options": [ ],
                  "bars": [ ]
                }
        \"\"\"
      """
    )
  }

  test("corrupted options parsing compile error") {
    compileErrors(
      """
        encode\"\"\"
                {
                  "ints": [ ],
                  "options": [ null, 42 ],
                  "bars": [ ]
                }
        \"\"\"
      """
    )
  }

  test("corrupted bars parsing compile error") {
    compileErrors(
      """
        encode\"\"\"
                {
                  "ints": [ -1 ],
                  "options": [ null ],
                  "bars": [ null ]
                }
        \"\"\"
      """
    )
  }
