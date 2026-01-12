package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*

case class Context(
    crossover: Crossover,
    mutation: Mutation,
    fitness: Fitness,
    selection: Selection
)

object Context:

    case class State(
        crossover: Crossover.State = Crossover.State(),
        mutation: Mutation.State = Mutation.State(),
        fitness: Fitness.State = Fitness.State(),
        selection: Selection.State = Selection.State.Pareto
    ) derives CanEqual, Json

    def init(state: State)(using Frame): Context < Sync =
        for
            crossover <- Crossover.init(state.crossover)
            mutation = Mutation.init(state.mutation)
            fitness <- Fitness.init(state.fitness)
            selection = Selection.init(state.selection)
        yield Context(crossover, mutation, fitness, selection)

end Context
