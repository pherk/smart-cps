module FHIR.Resources.Patient.Problem exposing
    ( Problem
    , decodeProblem
    , encodeProblem
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode


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


decodeProblem : Decoder Problem
decodeProblem =
    Decode.succeed Problem
        |> required "url" Decode.string
        |> required "valueAnnotation" decodeProblemValueAnnotation


decodeProblemValueAnnotationAuthorReference : Decoder ProblemValueAnnotationAuthorReference
decodeProblemValueAnnotationAuthorReference =
    Decode.succeed ProblemValueAnnotationAuthorReference
        |> required "reference" Decode.string
        |> required "display" Decode.string


decodeProblemValueAnnotation : Decoder ProblemValueAnnotation
decodeProblemValueAnnotation =
    Decode.succeed ProblemValueAnnotation
        |> required "authorReference" decodeProblemValueAnnotationAuthorReference
        |> required "time" Decode.string
        |> required "text" Decode.string


encodeProblem : Problem -> Encode.Value
encodeProblem record =
    Encode.object
        [ ( "url", Encode.string <| record.url )
        , ( "valueAnnotation", encodeProblemValueAnnotation <| record.valueAnnotation )
        ]


encodeProblemValueAnnotationAuthorReference : ProblemValueAnnotationAuthorReference -> Encode.Value
encodeProblemValueAnnotationAuthorReference record =
    Encode.object
        [ ( "reference", Encode.string <| record.reference )
        , ( "display", Encode.string <| record.display )
        ]


encodeProblemValueAnnotation : ProblemValueAnnotation -> Encode.Value
encodeProblemValueAnnotation record =
    Encode.object
        [ ( "authorReference", encodeProblemValueAnnotationAuthorReference <| record.authorReference )
        , ( "time", Encode.string <| record.time )
        , ( "text", Encode.string <| record.text )
        ]
