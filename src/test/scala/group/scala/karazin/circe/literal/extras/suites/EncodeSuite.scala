//package group.scala.karazin.circe.literal.extras.suites
//
//import cats.implicits._
//import io.circe.syntax._
//import io.circe.parser
//import io.circe.{Json, JsonObject}
//import org.scalacheck._
//import org.scalacheck.Prop._
//import scala.compiletime.testing.typeCheckErrors
//
//import group.scala.karazin.circe.literal.extras.macros
//import group.scala.karazin.circe.literal.extras.model.{given, _}
//import group.scala.karazin.circe.literal.extras.model._
//import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}
//
//class EncodeSuite extends munit.ScalaCheckSuite:
//
//  test("raw json parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    lazy val result: Json =
//      encode"""
//              {
//                "int": 42,
//                "bar": {
//                  "str": "str",
//                  "bool": true,
//                  "unit": { }
//                 },
//                "buzzes": [
//                  {
//                    "int": 42,
//                    "bool": true,
//                    "short": 4
//                  },
//                  {
//                    "int": 42,
//                    "bool": false,
//                    "short": 4
//                  }
//                ],
//                "qux": {
//                  "bool": true
//                }
//              }
//              """
//
//    val expected =
//      parser.parse(
//        s"""
//        {
//          "int": 42,
//          "bar": {
//             "str": "str",
//             "bool": true,
//             "unit": { }
//          },
//          "buzzes": [
//            {
//              "int": 42,
//              "bool": true,
//              "short": 4
//            },
//            {
//              "int": 42,
//              "bool": false,
//              "short": 4
//            }
//          ],
//          "qux": {
//            "bool": true
//          }
//        }
//        """
//      ).toOption.get
//
//    assert(result == expected)
//
//  }
//
//  property("inlined boolean parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (bool: Boolean) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": $bool,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": $bool,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": $bool,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "bool": $bool
//                  }
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": 42,
//            "bar": {
//               "str": "str",
//               "bool": $bool,
//               "unit": { }
//            },
//            "buzzes": [
//              {
//                "int": 42,
//                "bool": $bool,
//                "short": 4
//              },
//              {
//                "int": 42,
//                "bool": $bool,
//                "short": 4
//              }
//            ],
//            "qux": {
//              "bool": $bool
//            }
//          }
//          """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("corrupted boolean value json object parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": "corrupted",
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "str": "str"
//                  }
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//
//  }
//
//  test("inlined int parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (int: Int) =>
//
//      val result =
//        encode"""
//                {
//                  "int": $int,
//                  "bar": {
//                    "str": "str",
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": $int,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": $int,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "int": $int
//                  }
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": $int,
//            "bar": {
//              "str": "str",
//              "bool": true,
//              "unit": { }
//             },
//            "buzzes": [
//              {
//                "int": $int,
//                "bool": true,
//                "short": 4
//              },
//              {
//                "int": $int,
//                "bool": false,
//                "short": 4
//              }
//            ],
//            "qux": {
//              "int": $int
//            }
//          }
//          """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("corrupted int value json object parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": "corrupted",
//                  "bar": {
//                    "str": "str",
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "str": "str"
//                  }
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//
//  }
//
//  test("inlined string parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (str: String) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": $str,
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "str": $str
//                  }
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//            {
//              "int": 42,
//              "bar": {
//                "str": "$str",
//                "bool": true,
//                "unit": { }
//               },
//              "buzzes": [
//                {
//                  "int": 42,
//                  "bool": true,
//                  "short": 4
//                },
//                {
//                  "int": 42,
//                  "bool": false,
//                  "short": 4
//                }
//              ],
//              "qux": {
//                "str": "$str"
//              }
//            }
//            """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("corrupted string value json object parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": false,
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": {
//                    "str": "str"
//                  }
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//
//  }
//
//  test("empty inlined json object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    val result =
//      encode"""
//              {
//                "int": 42,
//                "bar": {
//                  "str": "str",
//                  "bool": true,
//                  "unit": { }
//                 },
//                "buzzes": [
//                  {
//                    "int": 42,
//                    "bool": true,
//                    "short": 4
//                  },
//                  {
//                    "int": 42,
//                    "bool": false,
//                    "short": 4
//                  }
//                ],
//                "qux": ${JsonObject.fromMap(Map.empty[String, Json])}
//              }
//              """
//
//    val expected =
//      parser.parse(
//        s"""
//          {
//            "int": 42,
//            "bar": {
//              "str": "str",
//              "bool": true,
//              "unit": { }
//             },
//            "buzzes": [
//              {
//                "int": 42,
//                "bool": true,
//                "short": 4
//              },
//              {
//                "int": 42,
//                "bool": false,
//                "short": 4
//              }
//            ],
//            "qux": { }
//          }
//          """
//      ).toOption.get
//
//    assertEquals(result, expected)
//
//  }
//
//  test("inlined json object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (jsonObject: JsonObject) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": $jsonObject
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//            {
//              "int": 42,
//              "bar": {
//                "str": "str",
//                "bool": true,
//                "unit": { }
//               },
//              "buzzes": [
//                {
//                  "int": 42,
//                  "bool": true,
//                  "short": 4
//                },
//                {
//                  "int": 42,
//                  "bool": false,
//                  "short": 4
//                }
//              ],
//              "qux": ${jsonObject.noSpaces}
//            }
//            """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("corrupted json object value json object parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": false,
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": "corrupted"
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//
//  }
//
//  test("empty buzzes array parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    val result =
//      encode"""
//              {
//                "int": 42,
//                "bar": {
//                  "str": "str",
//                  "bool": true,
//                  "unit": { }
//                 },
//                "buzzes": [],
//                "qux": { }
//              }
//              """
//
//    lazy val expected: Json =
//      parser.parse(
//        s"""
//          {
//            "int": 42,
//            "bar": {
//              "str": "str",
//              "bool": true,
//              "unit": { }
//             },
//            "buzzes": [],
//            "qux": { }
//          }
//          """
//      ).toOption.get
//
//      assertEquals(result, expected)
//  }
//
//  test("inlined buzzes array parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (buzzes: List[Buzz]) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": true,
//                    "unit": { }
//                   },
//                  "buzzes": $buzzes,
//                  "qux": { }
//                }
//                """
//
//      lazy val expected: Json =
//        parser.parse(
//          s"""
//            {
//              "int": 42,
//              "bar": {
//                "str": "str",
//                "bool": true,
//                "unit": { }
//               },
//              "buzzes": [${buzzes map { buzz =>
//                s"""{
//                  "int": ${buzz.int},
//                  "bool": ${buzz.bool} ,
//                  "short": ${buzz.short}
//                }""" } mkString ", "
//              }],
//              "qux": { }
//            }
//            """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//
//  }
//
//  test("corrupted buzzes array parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": true,
//                    "unit": { }
//                  },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": "corrupted",
//                      "short": 4
//                    }
//                  ],
//                  "qux": { }
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//
//  }
//
//  test("empty bar parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (str: String) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": { }
//                }
//                """
//
//      lazy val expected: Json =
//        parser.parse(
//          s"""
//            {
//              "int": 42,
//              "buzzes": [
//                {
//                  "int": 42,
//                  "bool": true,
//                  "short": 4
//                },
//                {
//                  "int": 42,
//                  "bool": false,
//                  "short": 4
//                }
//              ],
//              "qux": { }
//            }
//            """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("inlined bar object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (bar: Bar) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": $bar,
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": { }
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": 42,
//            "bar": {
//              "str": "${bar.str}",
//              "bool": ${bar.bool},
//              "unit": { }
//            },
//            "buzzes": [
//              {
//                "int": 42,
//                "bool": true,
//                "short": 4
//              },
//              {
//                "int": 42,
//                "bool": false,
//                "short": 4
//              }
//            ],
//            "qux": { }
//          }
//          """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("inlined bar-like object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (barLike: BarLike) =>
//
//      val result =
//        encode"""
//                {
//                  "int": 42,
//                  "bar": $barLike,
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": { }
//                }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": 42,
//            "bar": {
//              "str": "${barLike.str}",
//              "bool": ${barLike.bool},
//              "unit": { }
//            },
//            "buzzes": [
//              {
//                "int": 42,
//                "bool": true,
//                "short": 4
//              },
//              {
//                "int": 42,
//                "bool": false,
//                "short": 4
//              }
//            ],
//            "qux": { }
//          }
//          """
//        ).toOption.get
//
//      assertEquals(result, expected)
//
//    }
//  }
//
//  test("corrupted bar object parsing compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    typeCheckErrors(
//      """
//        encode""""" + """"
//                {
//                  "int": 42,
//                  "bar": {
//                    "str": "str",
//                    "bool": "corrupted",
//                    "unit": { }
//                  },
//                  "buzzes": [
//                    {
//                      "int": 42,
//                      "bool": true,
//                      "short": 4
//                    },
//                    {
//                      "int": 42,
//                      "bool": false,
//                      "short": 4
//                    }
//                  ],
//                  "qux": { }
//                }
//        """"" + """"
//      """
//    ).headOption match
//      case Some(error) => assert(error.message.startsWith("Encode error:"))
//      case _           => fail("No compilation error was found.")
//  }
//
//  test("inlined int, bar, buzzes and qux into foo object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (int: Int, bar: Option[Bar], buzzes: List[Buzz], qux: JsonObject) =>
//
//      val result =
//        encode"""{
//                    "int": $int,
//                    "bar": $bar,
//                    "buzzes": $buzzes,
//                    "qux": $qux
//                 }
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": $int,
//            ${bar.fold(""""bar": null,""") { bar =>
//              s"""
//                "bar": {
//                  "str": "${bar.str}",
//                  "bool": ${bar.bool},
//                  "unit": { }
//                },
//              """
//            }}
//            "buzzes": [${buzzes map { buzz =>
//              s"""
//               {
//                  "int": ${buzz.int},
//                  "bool": ${buzz.bool},
//                  "short": ${buzz.short}
//               }
//               """} mkString ","
//            }],
//            "qux": ${qux.noSpaces}
//          }
//          """
//        )
//
//      assertEquals(result, expected.toOption.get)
//
//    }
//  }
//
//  test("inlined foo object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (foo: Foo) =>
//
//      val result =
//        encode"""
//                $foo
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": ${foo.int},
//            ${foo.bar.fold(""""bar": null,""") { bar =>
//              s"""
//                "bar": {
//                  "str": "${bar.str}",
//                  "bool": ${bar.bool},
//                  "unit": { }
//                },
//              """
//            }}
//            "buzzes": [${foo.buzzes map { buzz =>
//              s"""
//               {
//                  "int": ${buzz.int},
//                  "bool": ${buzz.bool},
//                  "short": ${buzz.short}
//               }
//               """} mkString ","
//            }],
//            "qux": ${foo.qux.noSpaces}
//          }
//          """
//        )
//
//      assertEquals(result, expected.toOption.get)
//
//    }
//  }
//
//  test("inlined foo-like object parsing") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Foo]('sc, 'args) }
//
//    forAll { (fooLike: FooLike) =>
//
//      val result =
//        encode"""
//                $fooLike
//                """
//
//      val expected =
//        parser.parse(
//          s"""
//          {
//            "int": ${fooLike.int},
//            "bar": {
//              "str": "${fooLike.bar.str}",
//              "bool": ${fooLike.bar.bool},
//              "unit": { }
//            },
//            "buzzes": [${fooLike.buzzes map { buzz =>
//              s"""
//               {
//                  "int": ${buzz.int},
//                  "bool": ${buzz.bool},
//                  "short": ${buzz.short}
//               }
//               """} mkString ","
//            }],
//            "qux": ${fooLike.qux.noSpaces}
//          }
//          """
//        )
//
//      assertEquals(result, expected.toOption.get)
//
//    }
//  }
