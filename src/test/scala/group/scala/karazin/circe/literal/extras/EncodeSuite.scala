package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject}
import org.scalacheck._
import org.scalacheck.Prop._

import group.scala.karazin.circe.literal.extras.model.{given, _}

object EncodeSuite:

  extension (inline sc: StringContext)

    inline def encode(inline args: Any*): Json =
      ${ macros.encode[Foo]('sc, 'args) }
  
  object generators:

    val genStr: Gen[String] = Gen.alphaStr
    
    val genJsonObject: Gen[JsonObject] = for {
      map <- Arbitrary.arbitrary[Map[String, String]]
    } yield JsonObject.fromMap(map collect { 
      case (key, value) if key.nonEmpty => key -> Json.fromString(value) 
    })
    
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
      int  <- Arbitrary.arbitrary[Int]
      bar  <- Arbitrary.arbitrary[Option[Bar]]
      buzz <- Arbitrary.arbitrary[List[Buzz]]
      qux  <- Arbitrary.arbitrary[JsonObject]
    } yield Foo(int, bar, buzz, qux)
    
    val genFooLike: Gen[FooLike] = for {
      int  <- Arbitrary.arbitrary[Int]
      bar  <- Arbitrary.arbitrary[Bar]
      buzz <- Arbitrary.arbitrary[List[Buzz]]
      qux  <- Arbitrary.arbitrary[JsonObject]
    } yield FooLike(int, bar, buzz, qux)

    given Arbitrary[String] = Arbitrary(genStr)
    given Arbitrary[JsonObject] = Arbitrary(genJsonObject)
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
                ],
                "qux": {
                  "str": "str"  
                }
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
            ],
            qux: {
              "str": "str"
            }
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
                  ],
                  "qux": {
                    "bool": $bool
                  }
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
            ],
            "qux": {
              "bool": $bool
            }
          }
          """
        ).toOption.get   

      assertEquals(result, expected)

    }
  }

  test("corrupted boolean value json object parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": 42,
                  "bar": {
                    "str": "str",
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
                  ],
                  "qux": {
                    "str": "str"
                  }
                }
        \"\"\"
      """
    )

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
                  ],
                  "qux": {
                    "int": $int
                  }
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
            ],
            "qux": {
              "int": $int
            }
          }
          """
        ).toOption.get  

      assertEquals(result, expected)

    }
  }

  test("corrupted int value json object parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": "corrupted",
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
                  ],
                  "qux": {
                    "str": "str"
                  }
                }
        \"\"\"
      """
    )

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
                  ],
                  "qux": {
                    "str": $str
                  }
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
              ],
              "qux": {
                "str": "$str"
              }
            }
            """  
        ).toOption.get  

      assertEquals(result, expected)

    }
  }

  test("corrupted string value json object parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": 42,
                  "bar": {
                    "str": false,
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
                  ],
                  "qux": {
                    "str": "str"
                  }
                }
        \"\"\"
      """
    )

  }
  
  test("empty inlined json object parsing") {

    val result =
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
                ],
                "qux": ${JsonObject.fromMap(Map.empty[String, Json])}
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
            ],
            "qux": { }
          }
          """  
      ).toOption.get  

    assertEquals(result, expected)
  
  }
  
  property("inlined json object parsing") {

    forAll { (jsonObject: JsonObject) =>
      
      val result =
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
                  ],
                  "qux": $jsonObject
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
              ],
              "qux": ${jsonObject.noSpaces}
            }
            """  
        ).toOption.get  

      assertEquals(result, expected)

    }
  }
  
  test("corrupted json object value json object parsing compile error") {

    compileErrors(
      """
        encode\"\"\"
                {
                  "int": 42,
                  "bar": {
                    "str": false,
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
                  ],
                  "qux": "corrupted"
                }
        \"\"\"
      """
    )

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
                "buzzes": [],
                "qux": { }
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
            "buzzes": [],
            "qux": { }
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
                  "buzzes": $buzzes,
                  "qux": { }
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
              }],
              "qux": { }
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
                      "bool": true
                    },
                    {
                      "int": 42,
                      "bool": "corrupted"
                    }
                  ],
                  "qux": { }
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
                  ],
                  "qux": { }
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
              ],
              "qux": { }
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
                  ],
                  "qux": { }
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
            ],
            "qux": { }
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
                  ],
                  "qux": { }
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
            ],
            "qux": { }
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
                  ],
                  "qux": { }
                }
        \"\"\"
      """
    )
  }  

  property("inlined int, bar, buzzes and qux into foo object parsing") {

    forAll { (int: Int, bar: Option[Bar], buzzes: List[Buzz], qux: JsonObject) =>

      val result =
        encode"""{
                    "int": $int,
                    "bar": $bar,
                    "buzzes": $buzzes,
                    "qux": $qux
                 }
                """

      val expected =
        parser.parse(
          s"""
          {
            "int": $int,
            ${bar.fold(""""bar": null,""") { bar =>
              s"""
                "bar": {
                  "str": "${bar.str}",
                  "bool": ${bar.bool}
                },
              """
            }}
            "buzzes": [${buzzes map { buzz =>
              s"""
               {
                  "int": ${buzz.int},
                  "bool": ${buzz.bool}
               }
               """} mkString ","
            }],
            "qux": ${qux.noSpaces}
          }
          """
        )

      assertEquals(result, expected.toOption.get)

    }
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
            }],
            "qux": ${foo.qux.noSpaces}
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
            "bar": {
              "str": "${fooLike.bar.str}",
              "bool": ${fooLike.bar.bool}
            },
            "buzzes": [${fooLike.buzzes map { buzz =>
              s"""
               {    
                  "int": ${buzz.int},
                  "bool": ${buzz.bool}
               } 
               """} mkString ","
            }],
            "qux": ${fooLike.qux.noSpaces}
          }
          """
        )   

      assertEquals(result, expected.toOption.get)

    }
  }

end EncodeSuite
