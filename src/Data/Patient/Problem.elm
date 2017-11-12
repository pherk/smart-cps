module Data.Patient.Problem 
    exposing
        ( Problem
        , decodeProblem
        , encodeProblem
        )

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline

type alias Problem =
    { url : String
    , valueAnnotation : ProblemValueAnnotation
    }

type alias ProblemValueAnnotationAuthorReference =
    { reference : String
    , display : String
    }

type alias ProblemValueAnnotation =
    { authorReference : ProblemValueAnnotationAuthorReference
    , time : String
    , text : String
    }

decodeProblem : Json.Decode.Decoder Problem
decodeProblem =
    Json.Decode.Pipeline.decode Problem
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "valueAnnotation" (decodeProblemValueAnnotation)

decodeProblemValueAnnotationAuthorReference : Json.Decode.Decoder ProblemValueAnnotationAuthorReference
decodeProblemValueAnnotationAuthorReference =
    Json.Decode.Pipeline.decode ProblemValueAnnotationAuthorReference
        |> Json.Decode.Pipeline.required "reference" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "display" (Json.Decode.string)

decodeProblemValueAnnotation : Json.Decode.Decoder ProblemValueAnnotation
decodeProblemValueAnnotation =
    Json.Decode.Pipeline.decode ProblemValueAnnotation
        |> Json.Decode.Pipeline.required "authorReference" (decodeProblemValueAnnotationAuthorReference)
        |> Json.Decode.Pipeline.required "time" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "text" (Json.Decode.string)

encodeProblem : Problem -> Json.Encode.Value
encodeProblem record =
    Json.Encode.object
        [ ("url",  Json.Encode.string <| record.url)
        , ("valueAnnotation",  encodeProblemValueAnnotation <| record.valueAnnotation)
        ]

encodeProblemValueAnnotationAuthorReference : ProblemValueAnnotationAuthorReference -> Json.Encode.Value
encodeProblemValueAnnotationAuthorReference record =
    Json.Encode.object
        [ ("reference",  Json.Encode.string <| record.reference)
        , ("display",  Json.Encode.string <| record.display)
        ]

encodeProblemValueAnnotation : ProblemValueAnnotation -> Json.Encode.Value
encodeProblemValueAnnotation record =
    Json.Encode.object
        [ ("authorReference",  encodeProblemValueAnnotationAuthorReference <| record.authorReference)
        , ("time",  Json.Encode.string <| record.time)
        , ("text",  Json.Encode.string <| record.text)
        ]
