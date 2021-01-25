package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Json, Encoder, ParsingFailure, ACursor, HCursor}
import org.scalacheck._
import org.scalacheck.Prop._

import model.{given, _}

object EncodeSuite:

  extension (inline sc: StringContext)

    inline def encode(inline args: Any*): Json =
      ${ macros.encode[Foo]('sc, 'args) }
  
  object generators:

    val genStr: Gen[String] = Gen.alphaStr
    
    val genBar: Gen[Bar] = for {
      str   <- Arbitrary.arbitrary[String]
      bool  <- Arbitrary.arbitrary[Boolean]
    } yield Bar(str, bool)

    val genBarLike: Gen[BarLike] = for {
      str   <- Arbitrary.arbitrary[String]
      bool  <- Arbitrary.arbitrary[Boolean]
    } yield BarLike(str, bool)
    
    val genBuzz: Gen[Buzz] = for {
      int   <- Arbitrary.arbitrary[Int]
      bool  <- Arbitrary.arbitrary[Boolean]
    } yield Buzz(int, bool)
    
    val genBuzzLike: Gen[BuzzLike] = for {
      int   <- Arbitrary.arbitrary[Int]
      bool  <- Arbitrary.arbitrary[Boolean]
    } yield BuzzLike(int, bool)
    
    val genFoo: Gen[Foo] = for {
      int   <- Arbitrary.arbitrary[Int]
      bar   <- Arbitrary.arbitrary[Option[Bar]]
      buzz  <- Arbitrary.arbitrary[List[Buzz]]
    } yield Foo(int, bar, buzz)
    
    val genFooLike: Gen[FooLike] = for {
      int   <- Arbitrary.arbitrary[Int]
      bar   <- Arbitrary.arbitrary[Option[Bar]]
      buzz  <- Arbitrary.arbitrary[List[Buzz]]
    } yield FooLike(int, bar, buzz)

    given Arbitrary[String] = Arbitrary(genStr)
    given Arbitrary[Buzz] = Arbitrary(genBuzz)
    given Arbitrary[BuzzLike] = Arbitrary(genBuzzLike)
    given Arbitrary[Bar] = Arbitrary(genBar)
    given Arbitrary[BarLike] = Arbitrary(genBarLike)
    given Arbitrary[Foo] = Arbitrary(genFoo)
    given Arbitrary[FooLike] = Arbitrary(genFooLike)

  end generators

end EncodeSuite

class EncodeSuite extends munit.ScalaCheckSuite:
  import EncodeSuite._
  import EncodeSuite.generators.{given, _}

  test("raw json parsing") {

    lazy val result: Json =
      encode"""
              {
                "int": 42,
                "bar": {
                  "str": "str",
                  "bool": true
                 },
                "buzzes": [
                  {
                    "int": 42,
                    "bool": true
                  },
                  {
                    "int": 42,
                    "bool": false
                  }
                ]
              }
              """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": 42,
            "bar": {
               "str": "str",
               "bool": true
            },
            "buzzes": [
              {
                "int": 42,
                "bool": true
              },
              {
                "int": 42,
                "bool": false
              }
            ]
          }
          """
        ).toOption.get                

      assertEquals(result, expected)

  }

  property("inlined boolean parsing") {

    forAll { (bool: Boolean) =>

      val result =
        encode"""
                {
                  "int": 42,
                  "bar": {
                    "str": "str",
                    "bool": $bool
                   },
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": $bool
                    },
                    {
                      "int": 42,
                      "bool": $bool
                    }
                  ]
                }
                """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": 42,
            "bar": {
               "str": "str",
               "bool": $bool
            },
            "buzzes": [
              {
                "int": 42,
                "bool": $bool
              },
              {
                "int": 42,
                "bool": $bool
              }
            ]
          }
          """
        ).toOption.get   

      assertEquals(result, expected)

    }
  }

  property("inlined int parsing") {

    forAll { (int: Int) =>

      val result =
        encode"""
                {
                  "int": $int,
                  "bar": {
                    "str": "str",
                    "bool": true
                   },
                  "buzzes": [
                    {
                      "int": $int,
                      "bool": true
                    },
                    {
                      "int": $int,
                      "bool": false
                    }
                  ]
                }
                """

      val expected =  
        parser.parse(            
          s"""
          {
            "int": $int,
            "bar": {
              "str": "str",
              "bool": true
             },
            "buzzes": [
              {
                "int": $int,
                "bool": true
              },
              {
                "int": $int,
                "bool": false
              }
            ]
          }
          """
        ).toOption.get  

      assertEquals(result, expected)

    }
  }

  property("inlined string parsing") {

    forAll { (str: String) =>

      val result =
        encode"""
                {
                  "int": 42,
                  "bar": {
                    "str": $str,
                    "bool": true
                   },
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": false
                    }
                  ]
                }
                """

      val expected =  
        parser.parse(  
          s"""
            {
              "int": 42,
              "bar": {
                "str": "$str",
                "bool": true
               },
              "buzzes": [
                {
                  "int": 42,
                  "bool": true
                },
                {
                  "int": 42,
                  "bool": false
                }
              ]
            }
            """  
        ).toOption.get  

      assertEquals(result, expected)

    }
  }

  test("empty buzzes array parsing") {

    val result =
      encode"""
              {
                "int": 42,
                "bar": {
                  "str": "str",
                  "bool": true
                 },
                "buzzes": []
              }
              """

    lazy val expected: Json =  
      parser.parse(  
        s"""
          {
            "int": 42,
            "bar": {
              "str": "str",
              "bool": true
             },
            "buzzes": []
          }
          """  
      ).toOption.get  

      assertEquals(result, expected)
  }

  property("inlined buzzes array parsing") {

    forAll { (buzzes: List[Buzz]) =>

      val result =
        encode"""
                {
                  "int": 42,
                  "bar": {
                    "str": "str",
                    "bool": true
                   },
                  "buzzes": $buzzes
                }
                """

      lazy val expected: Json =  
        parser.parse(  
          s"""
            {
              "int": 42,
              "bar": {
                "str": "str",
                "bool": true
               },
              "buzzes": [${buzzes map { buzz => 
                s"""{
                  "int": ${buzz.int}, 
                  "bool": ${buzz.bool} 
                }""" } mkString ", "
              }]
            }
            """  
        ).toOption.get

      assertEquals(result, expected)

    }

  }

  test("corrupted buzzes array parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": 42,
                  "bar": {
                    "int": 42,
                    "bool": true
                  },
                  "buzzes": [
                    {
                      "int": 42,
                      "corrupted": "corrupted"
                    },
                    {
                      "int": 42,
                      "bool": "corrupted"
                    }
                  ]
                }
        \"\"\"
      """        
    )

  }
  
  property("empty bar parsing") {

    forAll { (str: String) =>

      val result =
        encode"""
                {
                  "int": 42,
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": false
                    }
                  ]
                }
                """

      lazy val expected: Json =  
        parser.parse(  
          s"""
            {
              "int": 42,
              "buzzes": [
                {
                  "int": 42,
                  "bool": true
                },
                {
                  "int": 42,
                  "bool": false
                }
              ]
            }
            """  
        ).toOption.get

      assertEquals(result, expected)

    }
  }

  property("inlined bar object parsing") {

    forAll { (bar: Bar) =>

      val result =
        encode"""
                {
                  "int": 42,
                  "bar": $bar,
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": false
                    }
                  ]
                }
                """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": 42,
            "bar": {
              "str": "${bar.str}",
              "bool": ${bar.bool}
            },
            "buzzes": [
              {
                "int": 42,
                "bool": true
              },
              {
                "int": 42,
                "bool": false
              }
            ]
          }
          """
        ).toOption.get                

      assertEquals(result, expected)

    }
  }

  property("inlined bar-like object parsing") {

    forAllNoShrink { (barLike: BarLike) =>

      val result = 
        encode"""
                {
                  "int": 42,
                  "bar": $barLike,
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": false
                    }
                  ]
                }
                """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": 42,
            "bar": {
              "str": "${barLike.str}",
              "bool": ${barLike.bool}
            },
            "buzzes": [
              {
                "int": 42,
                "bool": true
              },
              {
                "int": 42,
                "bool": false
              }
            ]
          }
          """
        ).toOption.get

      assertEquals(result, expected)

    }
  }

  test("corrupted bar object parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": 42,
                  "bar": {
                    "int": 42,
                    "bool": "corrupted"
                  },
                  "buzzes": [
                    {
                      "int": 42,
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": false
                    }
                  ]
                }
        \"\"\"
      """
    )
  }  
    
  property("inlined foo object parsing") {

    forAll { (foo: Foo) =>

      val result =
        encode"""
                $foo
                """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": ${foo.int},
            ${foo.bar.fold(""""bar": null,""") { bar =>
              s"""
                "bar": {
                  "str": "${bar.str}",
                  "bool": ${bar.bool}
                }, 
              """ 
            }}
            "buzzes": [${foo.buzzes map { buzz => 
              s"""
               {    
                  "int": ${buzz.int},
                  "bool": ${buzz.bool}
               } 
               """} mkString ","
            }]
          }
          """
        )   

      assertEquals(result, expected.toOption.get)

    }
  }

  property("inlined foo-like object parsing") {

    forAll { (fooLike: FooLike) =>

      val result =
        encode"""
                $fooLike
                """

      val expected =  
        parser.parse(  
          s"""
          {
            "int": ${fooLike.int},
            ${fooLike.bar.fold(""""bar": null,""") { bar =>
              s"""
                "bar": {
                  "str": "${bar.str}",
                  "bool": ${bar.bool}
                }, 
              """ 
            }}
            "buzzes": [${fooLike.buzzes map { buzz => 
              s"""
               {    
                  "int": ${buzz.int},
                  "bool": ${buzz.bool}
               } 
               """} mkString ","
            }]
          }
          """
        )   

      assertEquals(result, expected.toOption.get)

    }
  }

end EncodeSuite
