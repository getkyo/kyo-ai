// package kyo

// import kyo.ai.Config

// class KyoAIApp extends KyoApp.Base[AI & Async & Scope & Abort[Throwable]]:

//     protected def timeout: Duration = Duration.Infinity

//     protected def handle[A](v: A < (AI & Async & Scope & Abort[Throwable]))(using Frame): A < (Async & Abort[Throwable]) =
//         AI.run(Scope.run(v))

//     def run[A](config: Config => Config)(v: => A < (AI & Async & Scope & Abort[Throwable]))(using Frame): Unit =
//         run(Config.use(config(_).enable(v)))

//     def run[A](config: Config)(v: => A < (AI & Async & Scope & Abort[Throwable]))(using Frame): Unit =
//         run(config.enable(v))

//     final override protected def run[A](v: => A < (AI & Async & Scope & Abort[Throwable]))(using Frame): Unit =
//         import AllowUnsafe.embrace.danger
//         initCode = initCode.appended(() =>
//             val result = IO.Unsafe.evalOrThrow(Abort.run(Async.runAndBlock(timeout)(handle[A](v))))
//             onResult(result)
//         )
//     end run
// end KyoAIApp
