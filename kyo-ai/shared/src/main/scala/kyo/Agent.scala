package kyo

import Agent.internal.*
import javax.swing.plaf.nimbus.State
import kyo.ai.json.description

/** A stateful, actor-based LLM entity that processes typed messages.
  *
  * The `Agent` type represents a persistent AI entity that combines Kyo's LLM capabilities with an actor-like concurrency model. Agents
  * provide a powerful abstraction for building autonomous, long-lived AI components that can maintain state, process messages, and interact
  * with users and systems through typed interfaces.
  *
  * Unlike simple function calls to an LLM, Agents are persistent entities with lifecycles. They're created, receive and process messages,
  * maintain state, and are eventually stopped. This persistence enables building complex AI systems that maintain context over time and can
  * participate in larger application architectures.
  *
  * ## Creating Agents
  *
  * The primary way to create an agent is with the `Agent.run` method, which provides a high-level interface for defining an agent's
  * behavior:
  *
  * {{{
  * val agent = Agent.run[Query, Error, Response, S](
  *   prompt = Prompt.init("You are a helpful research assistant."),
  *   tools = Seq(weatherTool, calculatorTool),
  *   thoughts = Seq(Thought.opening[Analysis], Thought.closing[Verification])
  * )(query => AI.gen[Response](query))
  * }}}
  *
  * For more advanced use cases requiring custom actor behavior, the lower-level `Agent.runBehavior` methods provide additional flexibility.
  *
  * ## Interacting with Agents
  *
  * Clients interact with agents through an ask pattern, which sends a message and awaits a response:
  *
  * {{{
  * val response: Response < (Async & Abort[Closed]) = agent.ask(query)
  * }}}
  *
  * ## Concurrency Model
  *
  * The actor model provides natural concurrency control, allowing multiple agents to operate independently and safely in a system. This
  * makes Agents ideal for applications like multi-agent systems, conversational interfaces, and autonomous AI components in larger
  * applications.
  *
  * @tparam Error
  *   The error type that the agent can produce
  * @tparam In
  *   The input type that the agent accepts
  * @tparam Out
  *   The output type that the agent returns
  */
opaque type Agent[+Error, In, Out] = Actor[Error, Message[In, Out], Any]

object Agent:

    /** A reference to an Agent that can receive messages.
      */
    opaque type Ref[In, Out] = Subject[Message[In, Out]]

    extension [In, Out](self: Ref[In, Out])
        def ask(in: In)(using Frame): Out < (Async & Abort[Closed]) =
            self.ask(Message(in, _))

    /** The context in which an agent operates, providing access to its internal actor state.
      */
    opaque type Context[In, Out] = Actor.Context[Message[In, Out]]

    extension [Error, In, Out](self: Agent[Error, In, Out])
        /** Sends a message to the agent and awaits a response.
          *
          * This is the primary way to interact with an agent. It sends the input message to the agent and returns a computation that will
          * eventually produce the agent's response.
          *
          * @param in
          *   The input message to send to the agent
          * @return
          *   A computation that will produce the agent's response
          */
        def ask(in: In)(using Frame): Out < (Async & Abort[Closed]) =
            self.ask(Message[In, Out](in, _))

    end extension

    /** Creates and runs an agent with the specified prompt, tools, and thoughts.
      *
      * This is the recommended way to create agents for most use cases. It provides a high-level interface for defining an agent's behavior
      * through a simple message-processing function.
      *
      * Example:
      * {{{
      * val agent = Agent.run[Query, Error, Response, S](
      *   prompt = Prompt.init("You are a helpful assistant."),
      *   tools = Seq(weatherTool, calculatorTool)
      * )(query => AI.gen[Response](query))
      * }}}
      *
      * @param prompt
      *   The initial prompt that defines the agent's behavior
      * @param tools
      *   The tools available to the agent
      * @param thoughts
      *   The thought processes the agent can use
      * @param f
      *   The function that processes each input message and produces an output
      * @return
      *   A new agent instance
      */
    def run[In: Tag](using Frame)[Error, Out: Tag, S](
        using Isolate.Contextual[S, IO]
    )(
        prompt: Prompt[S] = Prompt.empty,
        tools: Seq[Tool[S]] = Nil,
        thoughts: Seq[Thought[S]] = Nil
    )(
        f: In => Out < (Abort[Error] & Context[In, Out] & AI & S)
    )(
        using
        Tag[Poll[Message[In, Out]]],
        Tag[Emit[Message[In, Out]]],
        Tag[Subject[Message[In, Out]]],
        Tag[Message[In, Out]]
    ): Agent[Error, In, Out] < (Resource & Async & S) =
        runBehavior[In](prompt, tools, thoughts)(Actor.receiveAll[Message[In, Out]](process(_, f)))

    /** Creates and runs an agent with default settings.
      *
      * This is a simplified version of the main `run` method that doesn't require explicit prompt, tools, or thoughts.
      *
      * @param f
      *   The function that processes each input message and produces an output
      * @return
      *   A new agent instance
      */
    def run[In: Tag](using Frame)[Error, Out: Tag, S](
        using Isolate.Contextual[S, IO]
    )(
        f: In => Out < (Abort[Error] & Context[In, Out] & AI & S)
    )(
        using
        Tag[Poll[Message[In, Out]]],
        Tag[Emit[Message[In, Out]]],
        Tag[Subject[Message[In, Out]]],
        Tag[Message[In, Out]]
    ): Agent[Error, In, Out] < (Resource & Async & S) =
        runBehavior(AI.run(Actor.receiveAll[Message[In, Out]](process(_, f))))

    /** Creates and runs an agent with a custom behavior.
      *
      * This is a lower-level method intended for advanced use cases where you need more control over the agent's internal actor behavior.
      * Most users should start with the higher-level `run` method instead.
      *
      * @param behavior
      *   The custom actor behavior that defines how the agent processes messages
      * @return
      *   A new agent instance
      */
    def runBehavior[In: Tag](using Frame)[Error, Out: Tag, S](
        using Isolate.Contextual[S, IO]
    )(
        behavior: Any < (Abort[Error] & Context[In, Out] & AI & S)
    )(
        using
        Tag[Poll[Message[In, Out]]],
        Tag[Emit[Message[In, Out]]],
        Tag[Subject[Message[In, Out]]],
        Tag[Message[In, Out]]
    ): Agent[Error, In, Out] < (Resource & Async & S) =
        Actor.run(AI.run(behavior))

    /** Creates and runs an agent with the specified prompt, tools, thoughts, and a custom behavior.
      *
      * This is a lower-level method intended for advanced use cases where you need more control over the agent's internal actor behavior
      * while still using the standard AI capabilities. Most users should start with the higher-level `run` method instead.
      *
      * @param prompt
      *   The initial prompt that defines the agent's behavior
      * @param tools
      *   The tools available to the agent
      * @param thoughts
      *   The thought processes the agent can use
      * @param behavior
      *   The custom actor behavior that defines how the agent processes messages
      * @return
      *   A new agent instance
      */
    def runBehavior[In: Tag](using Frame)[Error, Out: Tag, S](
        using Isolate.Contextual[S, IO]
    )(
        prompt: Prompt[S] = Prompt.empty,
        tools: Seq[Tool[S]] = Nil,
        thoughts: Seq[Thought[S]] = Nil
    )(
        behavior: Any < (Abort[Error] & Context[In, Out] & AI & S)
    )(
        using
        Tag[Poll[Message[In, Out]]],
        Tag[Emit[Message[In, Out]]],
        Tag[Subject[Message[In, Out]]],
        Tag[Message[In, Out]]
    ): Agent[Error, In, Out] < (Resource & Async & S) =
        runBehavior {
            behavior.handle(
                Prompt.enable(prompt),
                Tool.enable(tools*),
                Thought.enable(thoughts*),
                AI.run
            )
        }

    /** Processes all available messages using the provided function.
      *
      * This method will receive and process all messages currently in the agent's mailbox, applying the given function to each input and
      * sending the result back to the sender.
      *
      * Note: This method is intended to be used within a custom behavior passed to `runBehavior`. It should not be called directly outside
      * of an agent's behavior definition.
      *
      * @param f
      *   The function to apply to each input message
      */
    def receiveAll[In](using Frame)[Out, S](f: In => Out < S)(
        using Tag[Poll[Message[In, Out]]]
    ): Unit < (Context[In, Out] & S) =
        Actor.receiveAll[Message[In, Out]](process(_, f))

    /** Processes up to a maximum number of messages using the provided function.
      *
      * This method will receive and process up to `max` messages from the agent's mailbox, applying the given function to each input and
      * sending the result back to the sender.
      *
      * Note: This method is intended to be used within a custom behavior passed to `runBehavior`. It should not be called directly outside
      * of an agent's behavior definition.
      *
      * @param max
      *   The maximum number of messages to process
      * @param f
      *   The function to apply to each input message
      */
    def receiveMax[In](max: Int)[Out, S](f: In => Out < S)(using Frame, Tag[Poll[Message[In, Out]]]): Unit < (Context[In, Out] & S) =
        Actor.receiveMax(max)(process(_, f))

    /** Processes messages in a loop until the loop is explicitly terminated.
      *
      * This method continuously receives and processes messages, applying the given function to each input. The function returns a
      * `Loop.Outcome` that determines whether to continue processing messages or terminate the loop.
      *
      * Note: This method is intended to be used within a custom behavior passed to `runBehavior`. It should not be called directly outside
      * of an agent's behavior definition.
      *
      * @param f
      *   The function to apply to each input message, returning a loop control outcome
      */
    def receiveLoop[In](using Frame)[Out, S](
        f: In => Loop.Outcome[Out, Unit] < S
    )(
        using Tag[Poll[Message[In, Out]]]
    ): Unit < (Context[In, Out] & S) =
        Actor.receiveLoop { msg =>
            f(msg.input).map {
                case continue: Loop.Continue[Out] @unchecked =>
                    msg.replyTo.send(continue._1).andThen(Loop.continue)
                case _ =>
                    Loop.done
            }
        }

    /** Processes messages in a stateful loop until the loop is explicitly terminated.
      *
      * This method continuously receives and processes messages, applying the given function to each input along with the current state.
      * The function returns a `Loop.Outcome2` that includes the next state and determines whether to continue processing messages or
      * terminate the loop.
      *
      * Note: This method is intended to be used within a custom behavior passed to `runBehavior`. It should not be called directly outside
      * of an agent's behavior definition.
      *
      * @param state
      *   The initial state for the loop
      * @param f
      *   The function to apply to the current state and each input message, returning a loop control outcome with the next state
      * @return
      *   The final state when the loop terminates
      */
    def receiveLoop[In](using Frame)[State, Out, S](state: State)(
        f: (State, In) => Loop.Outcome2[State, Out, Unit] < S
    )(
        using Tag[Poll[Message[In, Out]]]
    ): State < (Context[In, Out] & S) =
        Actor.receiveLoop(state) { (msg, state) =>
            f(state, msg.input).map {
                case continue: Loop.Continue2[State, Out] @unchecked =>
                    msg.replyTo.send(continue._2).andThen(Loop.continue(continue._1))
                case _ =>
                    Loop.done(state)
            }
        }

    /** Retrieves a reference to the current agent within its own context.
      *
      * This allows an agent to reference itself, which is useful for self-recursive operations or for passing a reference to itself to
      * other components.
      *
      * @return
      *   A reference to the current agent
      */
    def self[In, Out](using Frame, Tag[Subject[Message[In, Out]]]): Ref[In, Out] < Context[In, Out] =
        Actor.self

    /** Performs an operation with a reference to the current agent.
      *
      * This is a convenience method that provides a reference to the current agent for use within a function, and ensures the operation is
      * performed within the agent's context.
      *
      * @param f
      *   The function to execute with the agent reference
      * @return
      *   The result of applying the function
      */
    def selfWith[In, Out](using Frame)[B, S](f: Ref[In, Out] => B < S)(
        using Tag[Subject[Message[In, Out]]]
    ): B < (Context[In, Out] & S) =
        Actor.selfWith(f)

    private[kyo] object internal:

        case class Message[In, Out](input: In, replyTo: Subject[Out])

        def process[In, Out, S](msg: Message[In, Out], f: In => Out < S)(using Frame) =
            f(msg.input).map(msg.replyTo.send)
    end internal
end Agent
