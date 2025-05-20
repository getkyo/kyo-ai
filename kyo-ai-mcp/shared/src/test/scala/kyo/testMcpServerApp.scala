import kyo.*

object testMcpServerApp extends KyoAIApp:

    case class Req(city: String)
    run {
        Mcp.Server.run(
            Tool.init[Req]("get_city_weather", "gets the weather of a city")(_ => "sunny!")
        )
    }
end testMcpServerApp
