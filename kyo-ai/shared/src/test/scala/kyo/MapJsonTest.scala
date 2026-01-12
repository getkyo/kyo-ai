package kyo

import kyo.ai.json.*

class MapJsonTest extends Test:

    "Map[String, Int]" - {
        "encode and decode" in run {
            val original = Map("a" -> 1, "b" -> 2, "c" -> 3)
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Map[String, Int]](encoded).getOrThrow
            assert(decoded == original)
        }

        "json schema" in run {
            val schema = Json.jsonSchema[Map[String, Int]]
            assert(schema.`type` == "array")
        }
    }

    "Map[Int, String]" - {
        "encode and decode" in run {
            val original = Map(1 -> "one", 2 -> "two", 3 -> "three")
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Map[Int, String]](encoded).getOrThrow
            assert(decoded == original)
        }

        "json schema" in run {
            val schema = Json.jsonSchema[Map[Int, String]]
            assert(schema.`type` == "array")
        }
    }

    "nested Map" - {
        "encode and decode" in run {
            case class Data(name: String, metadata: Map[String, Int]) derives Json, CanEqual
            val original = Data("test", Map("x" -> 10, "y" -> 20))
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Data](encoded).getOrThrow
            assert(decoded == original)
        }
    }
end MapJsonTest
