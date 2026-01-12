# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

kyo-ai is a Scala 3 library that extends the Kyo effect system with AI/LLM capabilities. The project provides composable, type-safe abstractions for interacting with large language models through a distinctive architecture that treats AI interactions as first-class effects.

## Build System

This is an SBT-based Scala project (Scala 3.7.0, SBT 1.10.1) configured as a cross-platform build with JVM support.

### Common Commands

- **Compile**: `sbt compile`
- **Run tests**: `sbt test`
- **Run specific test**: `sbt "testOnly *AITest"` or `sbt "testOnly *TestClassName"`
- **Format code**: `sbt scalafmt` (or `sbt scalafmtAll`)
- **Format check**: `sbt scalafmtCheck`
- **Run build**: `sbt clean compile test`

### Module Structure

The project is organized into these SBT modules:
- `kyo-ai`: Core AI module with LLM effects, agents, tools, prompts, and thoughts
- `kyo-ai-mcp`: Model Context Protocol integration (currently commented out)
- `kyo-ai-copilot`: Copilot-related functionality
- `kyo-ai-tlstm`: LSTM-based AI components
- `kyo-neotypes`: Neo4j graph database integration
- `kyo-container`: Container management for tests
- `kyo-tastyquery`: TASTy query support for compile-time reflection

The default SBT project is set based on the `platform` system property (defaults to JVM).

## Core Architecture

The library follows a distinctive effect-driven design where AI capabilities are effects that compose with other Kyo effects.

### AI Effect (`kyo.AI`)

The `AI` effect is an opaque type combining `Var[Context] & Async`, meaning:
- It automatically tracks and manages conversation context through the computation
- Context updates persist across operations within the same AI computation
- The AI instance IS the computation itself, not a service handle

**Key insight**: Since AI extends `Var[Context]`, the conversation history is managed as mutable state that flows through the effect composition.

Entry points:
- `AI.run(computation)`: Run with empty context
- `AI.run(config)(computation)`: Run with specific configuration
- `AI.gen[A]`: Generate typed response (core generation method)
- `AI.forget(computation)`: Run computation but discard context changes (transaction-like)
- `AI.fresh(computation)`: Run with new empty context

### Agents (`kyo.Agent`)

Agents are stateful, actor-based LLM entities: `opaque type Agent[+Error, In, Out] = Actor[Error, Message[In, Out], Any]`

Agents combine:
- Actor concurrency model for message processing
- Persistent conversation state
- Lifecycle management (created, running, stopped)

Create agents with:
- `Agent.run[In, Out](prompt, tools, thoughts)(messageHandler)`: High-level API
- `Agent.runBehavior(behavior)`: Low-level API with custom actor behavior

### Tools (`kyo.Tool`)

Tools are type-safe functions that LLMs can invoke. The system automatically:
- Derives JSON schemas from Scala types (using ZIO Schema)
- Registers tools when enabled
- Parses and dispatches tool calls
- Serializes results

Each tool can have an integrated `Prompt` that provides specialized instructions for using that tool. Tool prompts are automatically included in the context when tools are enabled.

Create tools: `Tool.init[In][Out, S](name, description, prompt)(implementation)`

### Prompts (`kyo.Prompt`)

Prompts use a dual-component structure to address instruction forgetting:
- **Primary instructions**: Placed at the start of context (detailed guidance)
- **Reminders**: "Float" at the end of context (concise summaries of critical instructions)

This design ensures models maintain instruction adherence in long conversations since reminders always appear immediately before generation.

Create prompts: `Prompt.init[S](primaryInstructions, reminders)`

The `p` string interpolator normalizes whitespace for multi-line prompts.

### Thoughts (`kyo.Thought`)

Structured thinking framework that embeds explicit reasoning into generation by extending the output JSON schema:
- `Opening` thoughts: Execute before main generation
- `Closing` thoughts: Execute after main generation

Each thought can have a processing function that executes after generation, enabling verification, metrics, or dependent generations.

Create thoughts:
- `Thought.opening[A](processingFunction)`
- `Thought.closing[A](processingFunction)`

### JSON Handling (`kyo.ai.json.Json`)

Type-safe JSON encoding/decoding using ZIO Schema:
- `Json[T]` trait provides schema, encode, decode
- Automatic schema derivation via `JsonDerive`
- Integration with ZIO JSON codecs

The `@doc` annotation adds documentation to JSON schemas sent to LLMs.

### Context Management (`kyo.ai.Context`)

Context tracks conversation history with messages:
- `SystemMessage`: Instructions/role definition
- `UserMessage`: User inputs (can include images)
- `AssistantMessage`: Model responses
- `ToolMessage`: Tool call results

Context operations:
- `context.systemMessage(content)`
- `context.userMessage(content, maybeImage)`
- `context.assistantMessage(content)`
- `context.merge(otherContext)`: Merges two contexts

### Configuration (`kyo.ai.Config`)

Configuration specifies LLM provider and parameters:
- Provider-specific configs: `Config.OpenAI.*`, `Config.Anthropic.*`
- Common parameters: temperature, maxTokens, topP, seed
- Configuration scoping: `AI.withConfig(config)(computation)`

## Effect Composition

The AI effects compose with Kyo's effect system:
- `AI & S`: Computation requiring AI and other effects S
- Effects are handled via `.handle()` or specific runners
- `Isolate[S, Sync, Any]` constraint ensures safe effect isolation

## Testing

Tests use ScalaTest with the custom `Test` base class (extends AsyncFlatSpec).
Test files: `kyo-ai/shared/src/test/scala/kyo/*Test.scala`

Common test pattern:
```scala
"test description" in run {
  AI.run(testConfig) {
    for
      result <- AI.gen[ExpectedType](input)
    yield assert(result.field == expectedValue)
  }
}
```

### Test Debugging Protocol

When debugging test failures, follow this systematic approach:

1. **Plan with TodoWrite**: Create a task list with one item per failing test suite
2. **Run tests**: Execute the test suite to identify all failures with `sbt "project <module>" test`
3. **Analyze each failure**:
   - Read the failing test code to understand expectations
   - Read the implementation being tested
   - Determine if it's a **code bug** or **test bug**
4. **Fix systematically**:
   - **Code bugs**: Fix the implementation to meet test expectations
   - **Test bugs**: Fix incorrect test expectations or invalid test data
   - **Never reduce test coverage** - maintain or improve coverage
5. **Verify incrementally**: Run `sbt "project <module>" "testOnly *TestName"` after each fix
6. **Final verification**: Run complete test suite to ensure no regressions
7. **Update TodoWrite**: Mark each task completed as you finish

**Common Issues in kyo-ai-ren:**
- Grid operations: Distinguish between object IDs from `grid.analysis` vs direct grid cell values
- Grid validation: Colors must be 0-9
- Commented code: Check for commented-out validations or logic that should be active
- Test data: Ensure test inputs use valid object IDs and color values

**Use `/debug-tests` slash command** to invoke this protocol automatically.

## Code Style

- Scala 3 syntax with `scala3` dialect
- 140 character line limit
- 4-space indentation
- End markers for blocks with 4+ lines
- Sorted imports with scalastyle ordering
- Format enforced via scalafmt on compile

## Dependencies

Core dependencies:
- Kyo framework (version 1.0-RC1+22-038de0d8-SNAPSHOT)
- ZIO Schema for JSON schema derivation
- STTP for HTTP client
- Playwright for browser automation

## Important Notes

- The repository uses forking in tests (`fork := true`) with JVM option `--add-opens=java.base/java.lang=ALL-UNNAMED`
- The AI computation model is stateful - context changes persist through the computation chain
- Use `AI.forget` when you need to make exploratory LLM calls without affecting the main conversation
- Tools and Prompts are enabled for computations via `.enable(v)` or `Tool.enable(tools*)(v)`
