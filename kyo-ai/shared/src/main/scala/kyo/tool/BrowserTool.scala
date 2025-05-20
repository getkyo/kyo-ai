package kyo.tool

import kyo.*
import kyo.Browser.Op
import kyo.ai.json.Json
import kyo.tool.BrowserTool.Interactive.OpResult
import zio.schema.Schema as ZSchema

object BrowserTool:

    case class Analysis(
        `Let me analyze what information is needed and where to find it`: String,
        `Let me reflect on the instructions to ensure compliance`: String
    )

    case class Input(
        analysis: Analysis,
        // `Let me analyze what information is needed and where to find it`: String,
        // `Let me reflect on the instructions to ensure compliance`: String,
        ops: List[Browser.Op[Any]]
        // `Have I followed all instructions?`: Boolean = true
    )

    case class Output(op: Op[Any], result: String)

    def tool(using Frame): Tool[Any] < (Resource & Async) =
        for
            meter       <- Meter.initMutex
            interactive <- Interactive.init
        yield Tool.init[Input](
            "browsing_tool",
            "A browser automation tool built on Playwright that enables web interaction through a series of operations.",
            prompt(interactive)
        ) { input =>
            Abort.recover(Abort.panic) {
                meter.run {
                    interactive.ask(Interactive.Request(input.ops, _)).map { res =>
                        val imageOps =
                            res.collect {
                                case OpResult(op, Result.Success(image: Image)) =>
                                    println(image.renderToConsole())
                                    op -> image
                            }
                        val allOps =
                            res.map {
                                case OpResult(op, Result.Success(image: Image)) =>
                                    Output(op, Result.Success("See image below.").show)
                                case OpResult(op, result) =>
                                    Output(op, result.show)
                            }
                        Kyo.foreachDiscard(imageOps) {
                            (op, image) => AI.userMessage(Json.encode(op), image)
                        }.andThen(allOps)
                    }
                }
            }
        }
    end tool

    type Interactive = Actor[Nothing, Interactive.Request, Unit]

    object Interactive:
        case class OpResult(op: Browser.Op[Any], result: Result[Throwable, Any])
        case class Request(ops: Seq[Browser.Op[Any]], replyTo: Subject[Chunk[OpResult]])

        def init(using Frame): Interactive < (Resource & Async) =
            Actor.receiveAll[Request] {
                case Request(ops, replyTo) =>
                    Kyo.foreach(ops) { op =>
                        Abort.run[Throwable](op.suspend.map((_, 0))).map { tup =>
                            OpResult(op, tup.map(_._1))
                        }
                    }.map(replyTo.send)
            }.handle(
                Browser.run,
                Actor.run(_)
            )
    end Interactive

    private def prompt(interactive: Interactive) =
        Prompt.init(
            Abort.recover(Abort.panic) {
                interactive.ask(Interactive.Request(Seq(Browser.Op.GetStatus), _)).map { status =>
                    p"""
                        CAPABILITIES & WHEN TO USE
                        ==========================
                        - Browser automation with persistent session for web navigation, interaction, and information extraction
                        - Executes operations (navigation, clicking, typing, scrolling), returns results and screenshots
                        - Handles cookie dialogs and can extract readable content from text-heavy pages
                        - Ideal for information gathering, form submission, and capturing web content
                        
                        USAGE GUIDELINES
                        ================
                        1. ANALYZE & PLAN
                        - Determine if information is publicly available and browsing is most efficient
                        - Identify authoritative sources and logical navigation steps
                        - Anticipate obstacles (login walls, popups, dynamic content)
                        
                        2. MANDATORY CONTENT EXTRACTION
                        - ALWAYS include explicit content retrieval operations after navigation
                        - NEVER navigate to a page without including a ReadableContent or Screenshot operation
                        - Text-heavy sites: PREFER Browser.Op.ReadableContent over screenshots
                        - Visual/interactive content: Use targeted screenshots when appropriate
                        
                        3. PERSIST THROUGH OBSTACLES
                        - Try multiple approaches when initial attempts fail
                        - Use alternative navigation methods, UI elements, or website sections
                        - Document thorough attempts when encountering limitations
                        
                        4. OPTIMIZE OPERATIONS
                        - Group related operations; only final state appears in screenshot
                        - Structure to show relevant content and use strategic scrolling
                        - ALWAYS follow Goto operations with either ReadableContent or Screenshot operations
                        
                        5. COMMUNICATE CLEARLY
                        - REMEMBER: The user CANNOT see the browser interactions or screenshots unless you share them
                        - NEVER assume the user can see what you're seeing in the browser
                        - ALWAYS include full URLs when referencing sources (not "the first website")
                        - Clearly attribute information to specific URLs when using multiple sources
                        - Include website name, full URL, and page title when available
                        - Explicitly state which URL provided which piece of information
                        - Be transparent about limitations encountered

                        CURRENT BROWSER STATUS
                        ======================
                        $status
                    """
                }
            },
            reminder =
                p"""
                    CRITICAL OPERATION REQUIREMENTS
                    ==============================
                    1. ALWAYS include explicit content retrieval (ReadableContent or Screenshot) after navigation operations
                    2. NEVER execute a Goto operation without a corresponding content retrieval operation
                    3. The user CANNOT see browser interactions, screenshots, or browsing results unless you explicitly share them
                    4. ALWAYS include complete URLs when citing information sources
                    5. NEVER make references like "the first website" or "the search results" - the user has NO visibility into what you're seeing
                """
        )

    given Frame       = Frame.internal
    given Json[Image] = Json.fromZio(ZSchema[String].transform(Image.fromBase64, _.base64))
end BrowserTool
