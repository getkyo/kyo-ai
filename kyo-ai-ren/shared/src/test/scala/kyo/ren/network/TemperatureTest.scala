package kyo.ren.network

import kyo.*
import kyo.Test
import kyo.ai.json.*
import kyo.ren.*

class TemperatureTest extends Test:

    "normalized should return default value for exact matches" in {
        assert(Temperature.precise.normalized == 0.1)
        assert(Temperature.focused.normalized == 0.4)
        assert(Temperature.balanced.normalized == 0.7)
        assert(Temperature.creative.normalized == 1.0)
        assert(Temperature.exploratory.normalized == 1.3)
    }

    "normalized should return initial value if within the category" in {
        val custom = Temperature.Focused(0.5)
        assert(custom.normalized == 0.5)
    }

    "decrease should lower temperature by 0.05" in {
        val temp = Temperature.Balanced(0.7)
        assert((temp.decrease.normalized - 0.65).abs < 0.01)
    }

    "decrease should not go below 0.1" in {
        val temp = Temperature.Precise(0.1)
        assert(temp.decrease.normalized == 0.1)
    }

    "increase should raise temperature by 0.05" in {
        val temp = Temperature.Balanced(0.7)
        assert(temp.increase.normalized == 0.75)
    }

    "increase should not go above 1.5" in {
        val temp = Temperature.Exploratory(1.5)
        assert(temp.increase.normalized == 1.5)
    }

end TemperatureTest
