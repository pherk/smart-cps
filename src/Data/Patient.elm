module Data.Patient
    exposing
        ( Patient
        , ID
        , Tag
        , PatientName
        , decodePatient
        , idParser
        , idToString
        , tagToString
        , tagDecoder
        )

import Data.Patient.Problem as Problem exposing(Problem)
import Data.Patient.Insurance as Insurance exposing (Insurance)
import Date exposing (Date)
import Html exposing (Attribute, Html)

import Json.Encode
import Json.Decode as Decode exposing(Decoder, string)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required, optional)
import UrlParser exposing ((</>), s, int, string, parseHash)

type alias Patient =
    { resourceType : String
    , id : String
    , meta : PatientMeta
    , lastModifiedBy : PatientLastModifiedBy
    , lastModified : String
    , multipleBirthInteger : String
    , multipleBirthBoolean : String
    , identifier : PatientIdentifier
    , name : PatientName
    , gender : String
    , birthDate : String
    , deceasedBoolean : String
    , address : PatientAddress
    , telecom : PatientTelecom
    , communication : PatientCommunication
--    , extension : List PatientExtension
    , managingOrganization : PatientManagingOrganization
    , active : String
    }

type PatientExtension
    = Problem
    | Insurance

type alias PatientMeta =
    { versionId : String
    }

type alias PatientLastModifiedBy =
    { reference : String
    , display : String
    }

type alias PatientIdentifierAssigner =
    { reference : String
    , display : String
    }

type alias PatientIdentifier =
    { use : String
    , type_ :  String
    , system : String
    , value : String
    , assigner : PatientIdentifierAssigner
    }

type alias PatientName =
    { use : String
    , family : String
    , given : String
    }

type alias PatientAddressPeriod =
    { start : String
    , end : String
    }

type alias PatientAddress =
    { use : String
    , line : String
    , city : String
    , state : String
    , postalCode : String
    , country : String
    , period : PatientAddressPeriod
    , preferred : String
    }

type alias PatientTelecom =
    { use : String
    , system : String
    , value : String
    , preferred : String
    }

type alias PatientCommunicationLanguageCoding =
    { system : String
    , code : String
    , display : String
    }

type alias PatientCommunicationLanguage =
    { coding : PatientCommunicationLanguageCoding
    , text : String
    }

type alias PatientCommunication =
    { language : PatientCommunicationLanguage
    , preferred : String
    }

type alias PatientManagingOrganization =
    { reference : String
    , display : String
    }

decodePatient : Decoder Patient
decodePatient =
    Pipeline.decode Patient
        |> Pipeline.required "resourceType" (Decode.string)
        |> Pipeline.required "id" (Decode.string)
        |> Pipeline.required "meta" (decodePatientMeta)
        |> Pipeline.required "lastModifiedBy" (decodePatientLastModifiedBy)
        |> Pipeline.required "lastModified" (Decode.string)
        |> Pipeline.required "multipleBirthInteger" (Decode.string)
        |> Pipeline.required "multipleBirthBoolean" (Decode.string)
        |> Pipeline.required "identifier" (decodePatientIdentifier)
        |> Pipeline.required "name" (decodePatientName)
        |> Pipeline.required "gender" (Decode.string)
        |> Pipeline.required "birthDate" (Decode.string)
        |> Pipeline.required "deceasedBoolean" (Decode.string)
        |> Pipeline.required "address" (decodePatientAddress)
        |> Pipeline.required "telecom" (decodePatientTelecom)
        |> Pipeline.required "communication" (decodePatientCommunication)
--        |> Pipeline.required "extension" (Decode.list decodeComplexType)
        |> Pipeline.required "managingOrganization" (decodePatientManagingOrganization)
        |> Pipeline.required "active" (Decode.string)

decodePatientMeta : Decoder PatientMeta
decodePatientMeta =
    Pipeline.decode PatientMeta
        |> Pipeline.required "versionId" (Decode.string)

decodePatientLastModifiedBy : Decoder PatientLastModifiedBy
decodePatientLastModifiedBy =
    Pipeline.decode PatientLastModifiedBy
        |> Pipeline.required "reference" (Decode.string)
        |> Pipeline.required "display" (Decode.string)

decodePatientIdentifierAssigner : Decoder PatientIdentifierAssigner
decodePatientIdentifierAssigner =
    Pipeline.decode PatientIdentifierAssigner
        |> Pipeline.required "reference" (Decode.string)
        |> Pipeline.required "display" (Decode.string)

decodePatientIdentifier : Decoder PatientIdentifier
decodePatientIdentifier =
    Pipeline.decode PatientIdentifier
        |> Pipeline.required "use" (Decode.string)
        |> Pipeline.optional "type" (Decode.string) ""
        |> Pipeline.required "system" (Decode.string)
        |> Pipeline.required "value" (Decode.string)
        |> Pipeline.required "assigner" (decodePatientIdentifierAssigner)

decodePatientName : Decoder PatientName
decodePatientName =
    Pipeline.decode PatientName
        |> Pipeline.required "use" (Decode.string)
        |> Pipeline.required "family" (Decode.string)
        |> Pipeline.required "given" (Decode.string)

decodePatientAddressPeriod : Decoder PatientAddressPeriod
decodePatientAddressPeriod =
    Pipeline.decode PatientAddressPeriod
        |> Pipeline.required "start" (Decode.string)
        |> Pipeline.required "end" (Decode.string)

decodePatientAddress : Decoder PatientAddress
decodePatientAddress =
    Pipeline.decode PatientAddress
        |> Pipeline.required "use" (Decode.string)
        |> Pipeline.required "line" (Decode.string)
        |> Pipeline.required "city" (Decode.string)
        |> Pipeline.required "state" (Decode.string)
        |> Pipeline.required "postalCode" (Decode.string)
        |> Pipeline.required "country" (Decode.string)
        |> Pipeline.required "period" (decodePatientAddressPeriod)
        |> Pipeline.required "preferred" (Decode.string)

decodePatientTelecom : Decoder PatientTelecom
decodePatientTelecom =
    Pipeline.decode PatientTelecom
        |> Pipeline.required "use" (Decode.string)
        |> Pipeline.required "system" (Decode.string)
        |> Pipeline.required "value" (Decode.string)
        |> Pipeline.required "preferred" (Decode.string)

decodePatientCommunicationLanguageCoding : Decoder PatientCommunicationLanguageCoding
decodePatientCommunicationLanguageCoding =
    Pipeline.decode PatientCommunicationLanguageCoding
        |> Pipeline.required "system" (Decode.string)
        |> Pipeline.required "code" (Decode.string)
        |> Pipeline.required "display" (Decode.string)

decodePatientCommunicationLanguage : Decoder PatientCommunicationLanguage
decodePatientCommunicationLanguage =
    Pipeline.decode PatientCommunicationLanguage
        |> Pipeline.required "coding" (decodePatientCommunicationLanguageCoding)
        |> Pipeline.required "text" (Decode.string)

decodePatientCommunication : Decoder PatientCommunication
decodePatientCommunication =
    Pipeline.decode PatientCommunication
        |> Pipeline.required "language" (decodePatientCommunicationLanguage)
        |> Pipeline.required "preferred" (Decode.string)

decodePatientManagingOrganization : Decoder PatientManagingOrganization
decodePatientManagingOrganization =
    Pipeline.decode PatientManagingOrganization
        |> Pipeline.required "reference" (Decode.string)
        |> Pipeline.required "display" (Decode.string)

encodePatient : Patient -> Json.Encode.Value
encodePatient record =
    Json.Encode.object
        [ ("resourceType",  Json.Encode.string <| record.resourceType)
        , ("id",  Json.Encode.string <| record.id)
        , ("meta",  encodePatientMeta <| record.meta)
        , ("lastModifiedBy",  encodePatientLastModifiedBy <| record.lastModifiedBy)
        , ("lastModified",  Json.Encode.string <| record.lastModified)
        , ("multipleBirthInteger",  Json.Encode.string <| record.multipleBirthInteger)
        , ("multipleBirthBoolean",  Json.Encode.string <| record.multipleBirthBoolean)
        , ("identifier",  encodePatientIdentifier <| record.identifier)
        , ("name",  encodePatientName <| record.name)
        , ("gender",  Json.Encode.string <| record.gender)
        , ("birthDate",  Json.Encode.string <| record.birthDate)
        , ("deceasedBoolean",  Json.Encode.string <| record.deceasedBoolean)
        , ("address",  encodePatientAddress <| record.address)
        , ("telecom",  encodePatientTelecom <| record.telecom)
        , ("communication",  encodePatientCommunication <| record.communication)
--        , ("extension",  Json.Encode.list <| List.map encodeComplexType <| record.extension)
        , ("managingOrganization",  encodePatientManagingOrganization <| record.managingOrganization)
        , ("active",  Json.Encode.string <| record.active)
        ]

encodePatientMeta : PatientMeta -> Json.Encode.Value
encodePatientMeta record =
    Json.Encode.object
        [ ("versionId",  Json.Encode.string <| record.versionId)
        ]

encodePatientLastModifiedBy : PatientLastModifiedBy -> Json.Encode.Value
encodePatientLastModifiedBy record =
    Json.Encode.object
        [ ("reference",  Json.Encode.string <| record.reference)
        , ("display",  Json.Encode.string <| record.display)
        ]

encodePatientIdentifierAssigner : PatientIdentifierAssigner -> Json.Encode.Value
encodePatientIdentifierAssigner record =
    Json.Encode.object
        [ ("reference",  Json.Encode.string <| record.reference)
        , ("display",  Json.Encode.string <| record.display)
        ]

encodePatientIdentifier : PatientIdentifier -> Json.Encode.Value
encodePatientIdentifier record =
    Json.Encode.object
        [ ("use",  Json.Encode.string <| record.use)
        , ("type",  Json.Encode.string <| record.type_)
        , ("system",  Json.Encode.string <| record.system)
        , ("value",  Json.Encode.string <| record.value)
        , ("assigner",  encodePatientIdentifierAssigner <| record.assigner)
        ]

encodePatientName : PatientName -> Json.Encode.Value
encodePatientName record =
    Json.Encode.object
        [ ("use",  Json.Encode.string <| record.use)
        , ("family",  Json.Encode.string <| record.family)
        , ("given",  Json.Encode.string <| record.given)
        ]

encodePatientAddressPeriod : PatientAddressPeriod -> Json.Encode.Value
encodePatientAddressPeriod record =
    Json.Encode.object
        [ ("start",  Json.Encode.string <| record.start)
        , ("end",  Json.Encode.string <| record.end)
        ]

encodePatientAddress : PatientAddress -> Json.Encode.Value
encodePatientAddress record =
    Json.Encode.object
        [ ("use",  Json.Encode.string <| record.use)
        , ("line",  Json.Encode.string <| record.line)
        , ("city",  Json.Encode.string <| record.city)
        , ("state",  Json.Encode.string <| record.state)
        , ("postalCode",  Json.Encode.string <| record.postalCode)
        , ("country",  Json.Encode.string <| record.country)
        , ("period",  encodePatientAddressPeriod <| record.period)
        , ("preferred",  Json.Encode.string <| record.preferred)
        ]

encodePatientTelecom : PatientTelecom -> Json.Encode.Value
encodePatientTelecom record =
    Json.Encode.object
        [ ("use",  Json.Encode.string <| record.use)
        , ("system",  Json.Encode.string <| record.system)
        , ("value",  Json.Encode.string <| record.value)
        , ("preferred",  Json.Encode.string <| record.preferred)
        ]

encodePatientCommunicationLanguageCoding : PatientCommunicationLanguageCoding -> Json.Encode.Value
encodePatientCommunicationLanguageCoding record =
    Json.Encode.object
        [ ("system",  Json.Encode.string <| record.system)
        , ("code",  Json.Encode.string <| record.code)
        , ("display",  Json.Encode.string <| record.display)
        ]

encodePatientCommunicationLanguage : PatientCommunicationLanguage -> Json.Encode.Value
encodePatientCommunicationLanguage record =
    Json.Encode.object
        [ ("coding",  encodePatientCommunicationLanguageCoding <| record.coding)
        , ("text",  Json.Encode.string <| record.text)
        ]

encodePatientCommunication : PatientCommunication -> Json.Encode.Value
encodePatientCommunication record =
    Json.Encode.object
        [ ("language",  encodePatientCommunicationLanguage <| record.language)
        , ("preferred",  Json.Encode.string <| record.preferred)
        ]

encodePatientManagingOrganization : PatientManagingOrganization -> Json.Encode.Value
encodePatientManagingOrganization record =
    Json.Encode.object
        [ ("reference",  Json.Encode.string <| record.reference)
        , ("display",  Json.Encode.string <| record.display)
        ]

-- IDENTIFIERS --


type ID
    = ID String


idParser : UrlParser.Parser (ID -> a) a
idParser =
    UrlParser.custom "ID" (Ok << ID)


idToString : ID -> String
idToString (ID id) = 
    id


-- TAGS --


type Tag
    = Tag String


tagToString : Tag -> String
tagToString (Tag id) =
    id


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map Tag Decode.string



