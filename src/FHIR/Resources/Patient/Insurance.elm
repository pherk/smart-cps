module FHIR.Resources.Patient.Insurance exposing
    ( Insurance
    , decodeInsurance
    , encodeInsurance
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode


type alias Insurance =
    { url : String
    , valueCodeableConcept : InsuranceValueCodeableConcept
    }


type alias InsuranceValueCodeableConceptCoding =
    { system : String
    , code : String
    , display : String
    }


type alias InsuranceValueCodeableConcept =
    { coding : InsuranceValueCodeableConceptCoding
    }


decodeInsurance : Decoder Insurance
decodeInsurance =
    Decode.succeed Insurance
        |> required "url" Decode.string
        |> required "valueCodeableConcept" decodeInsuranceValueCodeableConcept


decodeInsuranceValueCodeableConceptCoding : Decoder InsuranceValueCodeableConceptCoding
decodeInsuranceValueCodeableConceptCoding =
    Decode.succeed InsuranceValueCodeableConceptCoding
        |> required "system" Decode.string
        |> required "code" Decode.string
        |> required "display" Decode.string


decodeInsuranceValueCodeableConcept : Decoder InsuranceValueCodeableConcept
decodeInsuranceValueCodeableConcept =
    Decode.succeed InsuranceValueCodeableConcept
        |> required "coding" decodeInsuranceValueCodeableConceptCoding


encodeInsurance : Insurance -> Encode.Value
encodeInsurance record =
    Encode.object
        [ ( "url", Encode.string <| record.url )
        , ( "valueCodeableConcept", encodeInsuranceValueCodeableConcept <| record.valueCodeableConcept )
        ]


encodeInsuranceValueCodeableConceptCoding : InsuranceValueCodeableConceptCoding -> Encode.Value
encodeInsuranceValueCodeableConceptCoding record =
    Encode.object
        [ ( "system", Encode.string <| record.system )
        , ( "code", Encode.string <| record.code )
        , ( "display", Encode.string <| record.display )
        ]


encodeInsuranceValueCodeableConcept : InsuranceValueCodeableConcept -> Encode.Value
encodeInsuranceValueCodeableConcept record =
    Encode.object
        [ ( "coding", encodeInsuranceValueCodeableConceptCoding <| record.coding )
        ]
