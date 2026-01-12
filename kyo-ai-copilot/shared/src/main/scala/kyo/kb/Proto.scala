// package kyo.proto

// import kyo.*

// type Symbol = "name" ~ String

// type Doc =
//     "name" ~ String &
//         "lines" ~ Chunk[String] &
//         "content" ~ String

// object Doc:

//     type Name =
//         "document" ~ Doc &
//             "line" ~ Int &
//             "columnStart" ~ Int &
//             "columnEnd" ~ Int &
//             "symbol" ~ Symbol

//     type Lines =
//         "document" ~ Doc &
//             "lineStart" ~ Int &
//             "lineEnd" ~ Int

// end Doc

// type Type =
//     "name" ~ Doc.Name &
//         "package" ~ Doc.Name &
//         "scaladoc" ~ Maybe[Doc.Lines] &
//         "signature" ~ Doc.Lines &
//         "accessible" ~ Boolean &
