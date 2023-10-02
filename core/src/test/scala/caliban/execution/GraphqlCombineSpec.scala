package caliban
package execution

import schema.Schema
import schema.Schema.auto._
import zio.test._
import zio.ZIO

object GraphqlCombineSpec extends ZIOSpecDefault {
  private case class Query1(one: Int)
  private case class Query2(two: Int)

  private val query1 = Query1(1)
  private val query2 = Query2(2)

  private val expectedRnd1 = // rendered API 1
    """|schema {
       |  query: Query1
       |}
       |
       |type Query1 {
       |  one: Int!
       |}""".stripMargin

  private val expectedRnd2 = // rendered API 2
    """|schema {
       |  query: Query2
       |}
       |
       |type Query2 {
       |  two: Int!
       |}""".stripMargin

  private val expectedRdnX = // rendered API combined
    """|schema {
       |  query: Query2
       |}
       |
       |type Query2 {
       |  one: Int!
       |  two: Int!
       |}""".stripMargin

  private val testReq1 = "query test1 { one }"
  private val testReq2 = "query test2 { two }"
  private val testReqX = "query testCombined { one two }"

  private val expectedRes1 = """{"one":1}"""
  private val expectedRes2 = """{"two":2}"""
  private val expectedResX = """{"one":1,"two":2}"""

  private def mkQueryVariations[Q](query: Q, name: String)(implicit sc: Schema[Any, Q]): Seq[(String, GraphQL[Any])] =
    Seq(
      name                     -> graphQL(RootResolver(query)),
      s"(=> $name)"            -> graphQL(RootResolver(() => query)),
      s"(Field => $name)"      -> graphQL(RootResolver((_: Field) => query)),
      s"ZIO($name)"            -> graphQL(RootResolver(ZIO.succeed(query))),
      s"(Field => ZIO($name))" -> graphQL(RootResolver((_: Field) => ZIO.succeed(query)))
    )

  private val query1variations = mkQueryVariations(query1, "query1")
  private val query2variations = mkQueryVariations(query2, "query2")

  private def checkApi(
    api: GraphQL[Any],
    expectedRendered: String,
    testRequest: String,
    expectedResponse: String
  ) =
    for {
      interpreter <- api.interpreter
      response    <- interpreter.execute(testRequest)
    } yield {
      import Assertion._

      val obtainedRendered = api.render
      assert(obtainedRendered)(equalTo(expectedRendered)) &&
      assert(response.errors)(isEmpty) &&
      assert(response.data.toString)(equalTo(expectedResponse))
    }

  override def spec = suite("CompositionSpec")(
    //
    // Test `query1` variations alone.
    //
    query1variations.map { case (name1, api1) =>
      test(s"Query1 alone: $name1")(checkApi(api1, expectedRnd1, testReq1, expectedRes1))
    },
    //
    // Test `query2` variations alone.
    //
    query2variations.map { case (name2, api2) =>
      test(s"Query2 alone: $name2")(checkApi(api2, expectedRnd2, testReq2, expectedRes2))
    },
    //
    // Test all `query1` and `query2` variations combined.
    //
    for {
      (name1, api1) <- query1variations
      (name2, api2) <- query2variations
    } yield test(s"Query1 and Query2 combined: $name1 |+| $name2") {
      checkApi(api1 |+| api2, expectedRdnX, testReqX, expectedResX)
    }
  )
}
