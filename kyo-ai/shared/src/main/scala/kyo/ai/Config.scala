package kyo.ai

import kyo.*
import kyo.ai.completion.*

final case class Config private (
    apiUrl: String,
    apiKey: Maybe[String],
    apiOrg: Maybe[String],
    provider: Config.Provider,
    modelName: String,
    modelMaxTokens: Int,
    temperature: Double = 0.7,
    maxTokens: Maybe[Int] = Absent,
    seed: Maybe[Int] = Absent,
    meter: Meter = Meter.Noop,
    timeout: Duration = 5.minutes,
    maxIterations: Int = 5,
    retrySchedule: Schedule = Schedule.repeat(10)
):
    def apiUrl(url: String): Config =
        copy(apiUrl = url)
    def apiKey(key: String): Config =
        copy(apiKey = Present(key))
    def apiOrg(org: String): Config =
        copy(apiOrg = Present(org))
    def temperature(temperature: Double): Config =
        copy(temperature = temperature.max(0).min(2))
    def maxTokens(maxTokens: Maybe[Int]): Config =
        copy(maxTokens = maxTokens)
    def seed(seed: Maybe[Int]): Config =
        copy(seed = seed)
    def meter(meter: Meter): Config =
        copy(meter = meter)
    def timeout(timeout: Duration): Config =
        copy(timeout = timeout)
    def maxIterations(max: Int): Config =
        copy(maxIterations = max)
    def retrySchedule(retrySchedule: Schedule): Config =
        copy(retrySchedule = retrySchedule)

    def model(provider: Config.Provider, modelName: String, modelMaxTokens: Int): Config =
        copy(
            provider = provider,
            modelName = modelName,
            modelMaxTokens = modelMaxTokens,
            apiUrl = provider.baseUrl
        )

    def enable[A, S](v: A < S)(using Frame): A < S =
        Config.enable(this)(v)

end Config

object Config:

    private[kyo] def read(key: String): Maybe[String] =
        Maybe.fromOption(
            sys.props.get(key).orElse(sys.env.get(key))
        )

    val default: Config =
        Provider.all
            .find(p => read(p.keyName).isDefined)
            .getOrElse(OpenAI)
            .default

    private val local = Local.init(default)

    def get(using Frame): Config < Any = local.get

    def use[A, S](f: Config => A < S)(using Frame): A < S = local.use(f)

    def enable[A, S](config: Config)(v: A < S)(using Frame): A < S =
        if config == null then
            println(111)
        local.let(config)(v)
    end enable

    def update[A, S](f: Config => Config)(v: A < S)(using Frame): A < S =
        local.update { c =>
            val r = f(c)
            if r == null then
                println(999)
            r
        }(v)

    def init(provider: Provider, modelName: String, modelMaxTokens: Int): Config =
        Config(
            provider.baseUrl,
            read(provider.keyName),
            read(provider.orgKey),
            provider,
            modelName,
            modelMaxTokens
        )

    trait Provider(
        val name: String,
        val baseUrl: String,
        val keyName: String,
        val completion: Completion
    ):
        val orgKey: String = keyName + "_ORG"

        def default: Config

    end Provider

    object Provider:

        def init(
            name: String,
            baseUrl: String,
            keyName: String,
            completion: Completion,
            modelName: String,
            modelMaxTokens: Int
        ): Provider =
            init(name, baseUrl, keyName, completion, keyName + "_ORG", modelName, modelMaxTokens)

        def init(
            name: String,
            baseUrl: String,
            keyName: String,
            completion: Completion,
            orgKey: String,
            modelName: String,
            modelMaxTokens: Int
        ): Provider =
            new Provider(name, baseUrl, keyName, completion):
                override val orgKey: String = orgKey
                def default: Config         = Config.init(this, modelName, modelMaxTokens)

        val all: Chunk[Provider] = Chunk(Anthropic, DeepSeek, OpenAI, Gemini, Groq)
    end Provider

    case object Anthropic extends Provider(
            "Anthropic",
            "https://api.anthropic.com/v1",
            "ANTHROPIC_API_KEY",
            AnthropicCompletion
        ):
        val sonnet_3_7 = init(this, "claude-3-7-sonnet-latest", 200000)
        val sonnet_3_5 = init(this, "claude-3-5-sonnet-latest", 200000)
        val haiku_3_5  = init(this, "claude-3-5-haiku-latest", 200000)

        val opus_3   = init(this, "claude-3-opus-latest", 200000)
        val sonnet_3 = init(this, "claude-3-sonnet-20240229", 200000)
        val haiku_3  = init(this, "claude-3-haiku-20240307", 200000)

        def default: Config = sonnet_3_5

        val all = Chunk(sonnet_3_5, haiku_3_5, opus_3, sonnet_3, haiku_3)
    end Anthropic

    case object DeepSeek extends Provider(
            "DeepSeek",
            "https://api.deepseek.com/v1",
            "DEEPSEEK_API_KEY",
            OpenAICompletion
        ):
        val deepseek_chat     = init(this, "deepseek-chat", 64000)
        val deepseek_reasoner = init(this, "deepseek-reasoner", 64000)

        def default: Config = deepseek_chat

        val all = Chunk(
            deepseek_chat,
            deepseek_reasoner
        )
    end DeepSeek

    case object Groq extends Provider(
            "Groq",
            "https://api.groq.com/openai/v1",
            "GROQ_API_KEY",
            OpenAICompletion
        ):
        // Production Models
        val llama_3_3_70b_versatile = init(this, "llama-3.3-70b-versatile", 32768)
        val llama_3_1_8b_instant    = init(this, "llama-3.1-8b-instant", 8192)
        val llama_guard_3_8b        = init(this, "llama-guard-3-8b", 8192)
        val llama3_70b_8192         = init(this, "llama3-70b-8192", 8192)
        val llama3_8b_8192          = init(this, "llama3-8b-8192", 8192)
        val mixtral_8x7b_32768      = init(this, "mixtral-8x7b-32768", 32768)

        // Preview Models
        val qwen_2_5_32b                          = init(this, "qwen-2.5-32b", 8192)
        val deepseek_r1_distill_qwen_32b          = init(this, "deepseek-r1-distill-qwen-32b", 16384)
        val deepseek_r1_distill_llama_70b_specdec = init(this, "deepseek-r1-distill-llama-70b-specdec", 16384)
        val llama_3_3_70b_specdec                 = init(this, "llama-3.3-70b-specdec", 8192)
        val llama_3_2_1b_preview                  = init(this, "llama-3.2-1b-preview", 8192)
        val llama_3_2_3b_preview                  = init(this, "llama-3.2-3b-preview", 8192)
        val llama_3_2_11b_vision_preview          = init(this, "llama-3.2-11b-vision-preview", 8192)
        val llama_3_2_90b_vision_preview          = init(this, "llama-3.2-90b-vision-preview", 8192)

        def default: Config = llama_3_3_70b_versatile

        val all = Chunk(
            // Production Models
            llama_3_3_70b_versatile,
            llama_3_1_8b_instant,
            llama_guard_3_8b,
            llama3_70b_8192,
            llama3_8b_8192,
            mixtral_8x7b_32768,

            // Preview Models
            qwen_2_5_32b,
            deepseek_r1_distill_qwen_32b,
            deepseek_r1_distill_llama_70b_specdec,
            llama_3_3_70b_specdec,
            llama_3_2_1b_preview,
            llama_3_2_3b_preview,
            llama_3_2_11b_vision_preview,
            llama_3_2_90b_vision_preview
        )
    end Groq

    case object OpenAI extends Provider(
            "OpenAI",
            "https://api.openai.com/v1",
            "OPENAI_API_KEY",
            OpenAICompletion
        ):
        val gpt_4_5_preview = init(this, "gpt-4.5-preview", 128000)
        val gpt_4o          = init(this, "gpt-4o", 128000)
        val gpt_4o_mini     = init(this, "gpt-4o-mini", 128000)
        val gpt_4           = init(this, "gpt-4", 8192)
        val gpt_4_turbo     = init(this, "gpt-4-turbo", 128000)
        val gpt_3_5_turbo   = init(this, "gpt-3.5-turbo", 4096)

        val o1      = init(this, "o1", 200000)
        val o3_mini = init(this, "o3-mini", 200000)

        def default: Config = gpt_4o

        val all = Chunk(
            gpt_4o,
            gpt_4o_mini,
            gpt_4,
            gpt_4_turbo,
            gpt_3_5_turbo,
            o1,
            o3_mini
        )
    end OpenAI

    case object Gemini extends Provider(
            "Gemini",
            "https://generativelanguage.googleapis.com/v1beta/openai/",
            "GEMINI_API_KEY",
            OpenAICompletion
        ):
        val gemini_2_flash = init(this, "gemini-2.0-flash-exp", 1048576)

        val gemini_1_5_flash    = init(this, "gemini-1.5-flash", 1048576)
        val gemini_1_5_flash_8b = init(this, "gemini-1.5-flash-8b", 1048576)
        val gemini_1_5_pro      = init(this, "gemini-1.5-pro", 2097152)

        def default: Config = gemini_1_5_pro

        val all = Chunk(
            gemini_2_flash,
            gemini_1_5_flash,
            gemini_1_5_flash_8b,
            gemini_1_5_pro
        )
    end Gemini

end Config
