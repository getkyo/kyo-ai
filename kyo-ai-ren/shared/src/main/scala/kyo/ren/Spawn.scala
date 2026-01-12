// package kyo.ren

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.network.*

// final class Spawn(
//     val init: Spawn.State,
//     private var activeMind: Option[Mind],
//     private var deployCount: Int = 0,
//     private var processCount: Int = 0,
//     private var lastDeployed: Option[Instant] = None,
//     private var lastProcessed: Option[Instant] = None
// ):
//     private[ren] def deploy(mindState: Mind.State)(using Frame): String < AI =
//         val width = Spawn.calculateWidth(mindState.neurons)
//         val depth = Spawn.calculateDepth(mindState.neurons)

//         if !init.deployable then
//             AI.fail(s"Spawn does not allow deployment")
//         else if width > init.maxWidth then
//             AI.fail(s"Network width $width exceeds max ${init.maxWidth}")
//         else if depth > init.maxDepth then
//             AI.fail(s"Network depth $depth exceeds max ${init.maxDepth}")
//         else if mindState.maxRecursions > init.maxRecursions then
//             AI.fail(s"Network recursions ${mindState.maxRecursions} exceeds max ${init.maxRecursions}")
//         else
//             for
//                 newMind <- Mind.init(mindState)
//                 now     <- Clock.now
//             yield Sync.defer {
//                 activeMind = Some(newMind)
//                 deployCount += 1
//                 lastDeployed = Some(now)
//                 s"Network '${mindState.name}' deployed"
//             }
//         end if
//     end deploy

//     private[ren] def process(inputs: List[Input])(using Frame): String < AI =
//         activeMind match
//             case Some(mind) =>
//                 for
//                     result <-
//                         if init.ephemeral then
//                             for
//                                 freshMind <- mind.state.map(Mind.init(_))
//                                 result    <- freshMind[String](inputs)
//                             yield result
//                         else
//                             mind[String](inputs)
//                     now <- Clock.now
//                 yield Sync.defer {
//                     processCount += 1
//                     lastProcessed = Some(now)
//                     result
//                 }
//             case None =>
//                 "Spawn has no deployed network. Use the deploy command first."

//     def state(using Frame): Spawn.State < Sync =
//         activeMind match
//             case Some(mind) =>
//                 mind.state.map(mindState => init.copy(initialMind = Some(mindState)))
//             case None =>
//                 init
// end Spawn

// object Spawn:

//     case class State(
//         @doc("Description of what this spawn is for")
//         description: String,
//         @doc("Whether deployment is allowed (false = fixed network)")
//         deployable: Boolean = true,
//         @doc("Whether to re-initialize mind from state before each process (stateless execution)")
//         ephemeral: Boolean = false,
//         @doc("Maximum parallel neurons at any level in spawned networks")
//         maxWidth: Int = 3,
//         @doc("Maximum sequential path length in spawned networks")
//         maxDepth: Int = 2,
//         @doc("Maximum iteration cycles in spawned networks")
//         maxRecursions: Int = 2,
//         @doc("Initial network configuration (None if spawn starts empty)")
//         initialMind: Option[Mind.State] = None
//     ) derives CanEqual, Json:
//         require(description.nonEmpty, "Spawn description cannot be empty")
//         require(maxWidth > 0, s"maxWidth must be positive, got $maxWidth")
//         require(maxDepth > 0, s"maxDepth must be positive, got $maxDepth")
//         require(maxRecursions > 0, s"maxRecursions must be positive, got $maxRecursions")
//         require(deployable || initialMind.isDefined, "Non-deployable spawns must have initialMind")
//     end State

//     def init(state: State)(using Frame): Spawn < Sync =
//         state.initialMind match
//             case Some(mindState) =>
//                 Mind.init(mindState).map(mind => Spawn(state, Some(mind)))
//             case None =>
//                 Kyo.lift(Spawn(state, None))

//     enum Command derives CanEqual:
//         case Deploy(
//             @doc("Index of the spawn to deploy to")
//             index: Int,
//             @doc("Network configuration to deploy")
//             network: Mind.State
//         )

//         case Process(
//             @doc("Index of the spawn to use")
//             index: Int,
//             @doc("Inputs to process")
//             inputs: List[Input]
//         )
//     end Command

//     enum Response derives CanEqual:
//         case Deployed(index: Int, message: String)
//         case Output(index: Int, result: String)
//         case Error(message: String)
//     end Response

//     def tool(spawns: List[Spawn])(using Frame): Tool[AI] =
//         if spawns.isEmpty then Tool.empty
//         else
//             val toolPrompt = Prompt.init(
//                 prompt =
//                     Sync.defer {
//                         val spawnInfo = spawns.zipWithIndex.map { (spawn, idx) =>
//                             val status = spawn.activeMind.map(_.name).getOrElse("empty")
//                             val mode   = if spawn.init.deployable then "deployable" else "fixed"
//                             val state  = if spawn.init.ephemeral then "stateless" else "stateful"
//                             val limits = s"${spawn.init.maxWidth}w×${spawn.init.maxDepth}d×${spawn.init.maxRecursions}r"
//                             val usage  = s"deployed:${spawn.deployCount} processed:${spawn.processCount}"
//                             s"  [$idx] ${spawn.init.description} | $status | $mode | $state | max:$limits | $usage"
//                         }.mkString("\n")

//                         p"""
//                         When using spawns:
//                         1. Deploy a network configuration to a spawn before processing (unless fixed)
//                         2. Deploying a new network replaces any existing network in that spawn
//                         3. Each spawn can hold one active network at a time
//                         4. Spawns enforce width (parallel neurons), depth (sequential path), and recursion limits
//                         5. Available spawns:
//                         $spawnInfo
//                         """
//                     },
//                 reminder =
//                     Sync.defer {
//                         val activeCount = spawns.count(_.activeMind.isDefined)
//                         s"Deploy network before processing (unless fixed). $activeCount/${spawns.size} spawns have active networks."
//                     }
//             )

//             Tool.init[Command](
//                 "spawn",
//                 "Deploy networks to spawns and process inputs through them",
//                 toolPrompt
//             ) { cmd =>
//                 cmd match
//                     case Command.Deploy(index, network) =>
//                         if index < 0 || index >= spawns.size then
//                             Kyo.lift(Response.Error(s"Invalid spawn index $index. Valid range: 0-${spawns.size - 1}"))
//                         else
//                             Abort.recover[Throwable] { err =>
//                                 Response.Error(s"Deployment failed: ${err.getMessage}")
//                             } {
//                                 spawns(index).deploy(network).map(msg => Response.Deployed(index, msg))
//                             }

//                     case Command.Process(index, input) =>
//                         if index < 0 || index >= spawns.size then
//                             Kyo.lift(Response.Error(s"Invalid spawn index $index. Valid range: 0-${spawns.size - 1}"))
//                         else
//                             Abort.recover[Throwable] { err =>
//                                 Response.Error(s"Processing failed: ${err.getMessage}")
//                             } {
//                                 spawns(index).process(input).map(result => Response.Output(index, result))
//                             }
//             }
//     end tool

//     private def calculateWidth(neurons: List[Neuron.State]): Int =
//         if neurons.isEmpty then 0
//         else
//             val levels = assignLevels(neurons)
//             levels.values.groupBy(identity).view.mapValues(_.size).values.max

//     private def calculateDepth(neurons: List[Neuron.State]): Int =
//         if neurons.isEmpty then 0
//         else
//             val levels = assignLevels(neurons)
//             if levels.isEmpty then 0 else levels.values.max + 1

//     private def assignLevels(neurons: List[Neuron.State]): Map[Neuron.Name, Int] =
//         val neuronMap = neurons.map(n => n.name -> n).toMap

//         def computeLevel(name: Neuron.Name, visited: Set[Neuron.Name]): Int =
//             if visited.contains(name) then 0
//             else
//                 neuronMap.get(name) match
//                     case None => 0
//                     case Some(neuron) =>
//                         if neuron.previous.isEmpty then 0
//                         else
//                             val prevLevels = neuron.previous.flatMap { prevName =>
//                                 neuronMap.get(prevName).map(_ => computeLevel(prevName, visited + name))
//                             }
//                             if prevLevels.isEmpty then 0 else prevLevels.max + 1

//         neurons.map(n => n.name -> computeLevel(n.name, Set.empty)).toMap
//     end assignLevels
// end Spawn
