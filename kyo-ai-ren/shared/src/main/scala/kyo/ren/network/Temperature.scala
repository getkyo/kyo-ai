package kyo.ren.network

import kyo.*
import kyo.ai.json.Json

enum Temperature(val defaultValue: Double) derives CanEqual, Json:
    protected val value: Double

    case Precise(value: Double)     extends Temperature(0.1)
    case Focused(value: Double)     extends Temperature(0.4)
    case Balanced(value: Double)    extends Temperature(0.7)
    case Creative(value: Double)    extends Temperature(1.0)
    case Exploratory(value: Double) extends Temperature(1.3)

    def normalized =
        val normalized = Temperature.fromValue(value)
        if normalized == this then value
        else defaultValue
    end normalized

    def decrease: Temperature =
        Temperature.fromValue((value - 0.05).max(0.1))

    def increase: Temperature =
        Temperature.fromValue((value + 0.05).min(1.5))

end Temperature

object Temperature:

    val precise     = Precise(0.1)
    val focused     = Focused(0.4)
    val balanced    = Balanced(0.7)
    val creative    = Creative(1.0)
    val exploratory = Exploratory(1.3)

    val default: Temperature = balanced

    private val categories = List(precise, focused, balanced, creative, exploratory)

    private def fromValue(v: Double): Temperature =
        val closest = categories.minBy(t => math.abs(t.defaultValue - v))
        closest match
            case _: Precise     => Precise(v)
            case _: Focused     => Focused(v)
            case _: Balanced    => Balanced(v)
            case _: Creative    => Creative(v)
            case _: Exploratory => Exploratory(v)
        end match
    end fromValue
end Temperature
