// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Eval.*

// class EvalJudgeLiveTest extends Test:

//     def loadJudgeMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "eval", "judge", None)

//     "CRITICAL: scores correct answer higher than incorrect answer" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 correctScore <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 2+2?", "4", "4")
//                 )))
//                 wrongScore <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 2+2?", "4", "5")
//                 )))
//             yield
//                 assert(
//                     correctScore.score > wrongScore.score,
//                     s"CRITICAL: Correct answer (4) should score higher than wrong answer (5). Got correct=${correctScore.score}, wrong=${wrongScore.score}"
//                 )
//                 assert(correctScore.score > 0.7, s"Correct answer should score high: ${correctScore.score}")
//                 assert(wrongScore.score < 0.5, s"Wrong answer should score low: ${wrongScore.score}")
//         }
//     }

//     "returns score between 0.0 and 1.0" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 result <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 5*3?", "15", "15")
//                 )))
//             yield assert(result.score >= 0.0 && result.score <= 1.0, s"Score should be in [0.0, 1.0]: ${result.score}")
//         }
//     }

//     "provides coherent reasoning for scores" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 result <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("Calculate 10-3", "7", "7")
//                 )))
//             yield
//                 assert(result.reasoning.nonEmpty, "Judge should provide reasoning")
//                 assert(result.reasoning.length > 10, s"Reasoning should be substantive: '${result.reasoning}'")
//         }
//     }

//     "handles exact matches with high scores" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 result <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is the capital of France?", "Paris", "Paris")
//                 )))
//             yield assert(result.score >= 0.9, s"Exact match should score very high: ${result.score}")
//         }
//     }

//     "handles semantic equivalence appropriately" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 result <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 6/2?", "3", "3.0")
//                 )))
//             yield assert(result.score >= 0.8, s"Semantically equivalent answers should score high: ${result.score}")
//         }
//     }

//     "handles partial correctness in multi-part answers" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 fullCorrect <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("List two even numbers", "2, 4", "2, 4")
//                 )))
//                 partialCorrect <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("List two even numbers", "2, 4", "2, 5")
//                 )))
//             yield
//                 assert(
//                     fullCorrect.score > partialCorrect.score,
//                     s"Fully correct should score higher than partially correct: full=${fullCorrect.score}, partial=${partialCorrect.score}"
//                 )
//                 assert(
//                     partialCorrect.score > 0.3,
//                     s"Partial correctness should get some credit: ${partialCorrect.score}"
//                 )
//         }
//     }

//     "handles completely wrong answers with low scores" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 result <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 10*10?", "100", "banana")
//                 )))
//             yield assert(result.score <= 0.2, s"Completely wrong answer should score very low: ${result.score}")
//         }
//     }

//     "evaluates based on correctness not verbosity" in run {
//         AI.run {
//             for
//                 judge     <- loadJudgeMind
//                 judgeInst <- Mind.init(judge)
//                 conciseScore <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 8+7?", "15", "15")
//                 )))
//                 verboseScore <- judgeInst[JudgeOutput](List(Json.encode(
//                     JudgeInput("What is 8+7?", "15", "The answer is 15, calculated by adding 8 and 7 together.")
//                 )))
//             yield
//                 val scoreDiff = Math.abs(conciseScore.score - verboseScore.score)
//                 assert(
//                     scoreDiff < 0.3,
//                     s"Both correct answers should score similarly regardless of verbosity: concise=${conciseScore.score}, verbose=${verboseScore.score}"
//                 )
//         }
//     }

// end EvalJudgeLiveTest
