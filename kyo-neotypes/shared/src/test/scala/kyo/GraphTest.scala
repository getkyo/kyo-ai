package kyo

import kyo.debug.Debug
import neotypes.GraphDatabase
import neotypes.generic.implicits.*
import neotypes.mappers.ResultMapper
import neotypes.model.query.QueryParam
import neotypes.syntax.all.*
import org.neo4j.driver.AuthTokens

class Neo4jTest extends Test:

    case class Person(name: String, age: Int)
    case class Relationship(source: String, target: String, relType: String)
    case class Count(count: Long)

    given ResultMapper[Person]       = ResultMapper.productDerive[Person]
    given ResultMapper[Relationship] = ResultMapper.productDerive[Relationship]
    given ResultMapper[Count]        = ResultMapper.productDerive[Count]

    val memgraphBoltPort = 7687

    val memgraphConfig = Container.Config(
        image = "memgraph/memgraph",
        ports = List(
            Container.Config.Port(memgraphBoltPort, memgraphBoltPort)
        ),
        waitFor = Container.Config.WaitFor.LogMessage("You are running Memgraph"),
        stopTimeout = 5.seconds
    )

    def withDB[A](v: A < Graph) =
        Log.withConsoleLogger {
            for
                container <- Container.init(memgraphConfig)
                uri    = s"bolt://localhost:$memgraphBoltPort"
                driver = GraphDatabase.asyncDriver[Graph.F](uri)
                result <- Graph.run(driver)(v)
            yield result
            end for
        }
    end withDB

    "query" - {
        "basic query" in run {
            withDB {
                for
                    _      <- Graph.execute("CREATE (p:Person {name: 'Alice', age: 30}) RETURN p")
                    people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                    _      <- Graph.execute("MATCH (n) DETACH DELETE n")
                yield
                    assert(people.size == 1)
                    assert(people.head.name == "Alice")
                    assert(people.head.age == 30)
            }
        }

        "query with params" in run {
            withDB {
                for
                    _ <- Graph.execute(
                        "CREATE (p:Person {name: $name, age: $age}) RETURN p",
                        Map("name" -> QueryParam("Bob"), "age" -> QueryParam(25))
                    )
                    people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                    _      <- Graph.execute("MATCH (n) DETACH DELETE n")
                yield
                    assert(people.size == 1)
                    assert(people.head.name == "Bob")
                    assert(people.head.age == 25)
            }
        }

        "query with custom factory" in run {
            withDB {
                for
                    _      <- Graph.execute("CREATE (p:Person {name: 'Charlie', age: 40}) RETURN p")
                    people <- Graph.query[Person](List, "MATCH (p:Person) RETURN p.name as name, p.age as age")
                    _      <- Graph.execute("MATCH (n) DETACH DELETE n")
                yield
                    assert(people.isInstanceOf[List[Person]])
                    assert(people.size == 1)
                    assert(people.head.name == "Charlie")
            }
        }

        "invalid query" in run {
            withDB {
                for
                    result1 <- Abort.run(Graph.execute("INVALID CYPHER QUERY"))
                    result2 <- Abort.run(Graph.query[Person]("INVALID CYPHER QUERY"))
                yield
                    assert(result1.isPanic)
                    assert(result2.isPanic)
            }
        }
    }

    "transaction" - {
        "successful commit" in run {
            withDB {
                for
                    result <- Graph.transaction {
                        for
                            _      <- Graph.execute("CREATE (p:Person {name: 'Dave', age: 35}) RETURN p")
                            people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                        yield people
                    }
                    people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                    _      <- Graph.execute("MATCH (n) DETACH DELETE n")
                yield
                    assert(result.size == 1)
                    assert(result.head.name == "Dave")
                    assert(people.size == 1)
                    assert(people.head.name == "Dave")
            }
        }

        "rollback on failure" in run {
            withDB {
                for
                    _ <- Graph.execute("CREATE (p:Person {name: 'Eve', age: 28}) RETURN p")
                    result <- Abort.run(Graph.transaction {
                        for
                            _      <- Graph.execute("CREATE (p:Person {name: 'Frank', age: 45}) RETURN p")
                            _      <- Abort.fail(new Exception("Transaction test failure"))
                            people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                        yield people
                    })
                    people <- Graph.query[Person]("MATCH (p:Person) RETURN p.name as name, p.age as age")
                    _      <- Graph.execute("MATCH (n) DETACH DELETE n")
                yield
                    assert(result.isFailure)
                    assert(people.size == 1)
                    assert(people.head.name == "Eve")
            }
        }
    }

end Neo4jTest
