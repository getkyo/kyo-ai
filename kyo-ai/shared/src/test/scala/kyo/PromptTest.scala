package kyo

import kyo.*
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description
import scala.collection.immutable.ListMap

class PromptTest extends Test:

    "prompt creation" in run {
        val prompt = Prompt.init("This is a primary instruction")
        assert(prompt != null)
    }

    "prompt with reminder" in run {
        val prompt = Prompt.init(
            "This is a primary instruction",
            "This is a reminder"
        )
        assert(prompt != null)
    }

    "string interpolator" in run {
        val text = p"""
            This is a multi-line
            instruction with spaces
            that should be normalized
        """

        assert(text == "This is a multi-line\ninstruction with spaces\nthat should be normalized")
    }

    "combine prompts" in run {
        val prompt1 = Prompt.init("First instruction")
        val prompt2 = Prompt.init("Second instruction")

        val combined = prompt1.andThen(prompt2)

        combined.prompts.map { prompts =>
            assert(prompts.size == 2)
            assert(prompts.contains("First instruction"))
            assert(prompts.contains("Second instruction"))
        }.handle(AI.run)
    }

    "combine prompts with reminders" in run {
        AI.run {
            val prompt1 = Prompt.init("First instruction", "First reminder")
            val prompt2 = Prompt.init("Second instruction", "Second reminder")

            val combined = prompt1.andThen(prompt2)

            for
                prompts   <- combined.prompts
                reminders <- combined.reminders
            yield
                assert(prompts.size == 2)
                assert(reminders.size == 2)
                assert(prompts.contains("First instruction"))
                assert(prompts.contains("Second instruction"))
                assert(reminders.contains("First reminder"))
                assert(reminders.contains("Second reminder"))
            end for
        }
    }

    "empty prompt" in run {
        AI.run {
            val empty = Prompt.empty
            for
                prompts   <- empty.prompts
                reminders <- empty.reminders
            yield
                assert(prompts.isEmpty)
                assert(reminders.isEmpty)
            end for
        }
    }

    "combine with empty prompt" in run {
        AI.run {
            val prompt   = Prompt.init("Test instruction", "Test reminder")
            val combined = prompt.andThen(Prompt.empty)

            for
                p1 <- prompt.prompts
                r1 <- prompt.reminders
                p2 <- combined.prompts
                r2 <- combined.reminders
            yield
                assert(p1 == p2)
                assert(r1 == r2)
            end for
        }
    }

end PromptTest
