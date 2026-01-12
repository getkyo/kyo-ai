package kyo.ai.completion

import kyo.*
import kyo.AI
import kyo.Tool
import kyo.ai.*
import kyo.ai.Context.*

trait Completion:

    def apply(
        config: Config,
        context: Context,
        tools: Chunk[Tool.internal.Info[?, ?, AI]]
    )(using Frame): AssistantMessage < (AI & Abort[Throwable])

end Completion
