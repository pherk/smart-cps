module Data.Patient.Insurance 
    exposing
        ( Insurance
        , decodeInsurance
        , encodeInsurance
        )

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline

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

decodeInsurance : Json.Decode.Decoder Insurance
decodeInsurance =
    Json.Decode.Pipeline.decode Insurance
        |> Json.Decode.Pipeline.required "url" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "valueCodeableConcept" (decodeInsuranceValueCodeableConcept)

decodeInsuranceValueCodeableConceptCoding : Json.Decode.Decoder InsuranceValueCodeableConceptCoding
decodeInsuranceValueCodeableConceptCoding =
    Json.Decode.Pipeline.decode InsuranceValueCodeableConceptCoding
        |> Json.Decode.Pipeline.required "system" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "code" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "display" (Json.Decode.string)

decodeInsuranceValueCodeableConcept : Json.Decode.Decoder InsuranceValueCodeableConcept
decodeInsuranceValueCodeableConcept =
    Json.Decode.Pipeline.decode InsuranceValueCodeableConcept
        |> Json.Decode.Pipeline.required "coding" (decodeInsuranceValueCodeableConceptCoding)

encodeInsurance : Insurance -> Json.Encode.Value
encodeInsurance record =
    Json.Encode.object
        [ ("url",  Json.Encode.string <| record.url)
        , ("valueCodeableConcept",  encodeInsuranceValueCodeableConcept <| record.valueCodeableConcept)
        ]

encodeInsuranceValueCodeableConceptCoding : InsuranceValueCodeableConceptCoding -> Json.Encode.Value
encodeInsuranceValueCodeableConceptCoding record =
    Json.Encode.object
        [ ("system",  Json.Encode.string <| record.system)
        , ("code",  Json.Encode.string <| record.code)
        , ("display",  Json.Encode.string <| record.display)
        ]

encodeInsuranceValueCodeableConcept : InsuranceValueCodeableConcept -> Json.Encode.Value
encodeInsuranceValueCodeableConcept record =
    Json.Encode.object
        [ ("coding",  encodeInsuranceValueCodeableConceptCoding <| record.coding)
        ]
