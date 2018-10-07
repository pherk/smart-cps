module FHIR.Resources.Patient exposing
    ( 
      Patient
    , PatientName
    , decodePatient
    , encodePatient
    )


import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Author exposing (Author)
import FHIR.Resources.Body as Body exposing (Body)
import FHIR.Resources.ID as ID exposing (ID)
import FHIR.Resources.Patient.Insurance as Insurance exposing (Insurance)
import FHIR.Resources.Patient.Problem as Problem exposing (Problem)
import FHIR.Resources.Tag as Tag exposing (Tag)
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode
import Markdown
import Profile exposing (Profile)
import Time
import Username as Username exposing (Username)
import Viewer exposing (Viewer)


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
    , type_ : String
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


-- Decoder

decodePatient : Decoder Patient
decodePatient =
    Decode.succeed Patient
        |> required "resourceType" Decode.string
        |> required "id" Decode.string
        |> required "meta" decodePatientMeta
        |> required "lastModifiedBy" decodePatientLastModifiedBy
        |> required "lastModified" Decode.string
        |> required "multipleBirthInteger" Decode.string
        |> required "multipleBirthBoolean" Decode.string
        |> required "identifier" decodePatientIdentifier
        |> required "name" decodePatientName
        |> required "gender" Decode.string
        |> required "birthDate" Decode.string
        |> required "deceasedBoolean" Decode.string
        |> required "address" decodePatientAddress
        |> required "telecom" decodePatientTelecom
        |> required "communication" decodePatientCommunication
        --        |> required "extension" (Decode.list decodeComplexType)
        |> required "managingOrganization" decodePatientManagingOrganization
        |> required "active" Decode.string


decodePatientMeta : Decoder PatientMeta
decodePatientMeta =
    Decode.succeed PatientMeta
        |> required "versionId" Decode.string


decodePatientLastModifiedBy : Decoder PatientLastModifiedBy
decodePatientLastModifiedBy =
    Decode.succeed PatientLastModifiedBy
        |> required "reference" Decode.string
        |> required "display" Decode.string


decodePatientIdentifierAssigner : Decoder PatientIdentifierAssigner
decodePatientIdentifierAssigner =
    Decode.succeed PatientIdentifierAssigner
        |> required "reference" Decode.string
        |> required "display" Decode.string


decodePatientIdentifier : Decoder PatientIdentifier
decodePatientIdentifier =
    Decode.succeed PatientIdentifier
        |> required "use" Decode.string
        |> optional "type" Decode.string ""
        |> required "system" Decode.string
        |> required "value" Decode.string
        |> required "assigner" decodePatientIdentifierAssigner


decodePatientName : Decoder PatientName
decodePatientName =
    Decode.succeed PatientName
        |> required "use" Decode.string
        |> required "family" Decode.string
        |> required "given" Decode.string


decodePatientAddressPeriod : Decoder PatientAddressPeriod
decodePatientAddressPeriod =
    Decode.succeed PatientAddressPeriod
        |> required "start" Decode.string
        |> required "end" Decode.string


decodePatientAddress : Decoder PatientAddress
decodePatientAddress =
    Decode.succeed PatientAddress
        |> required "use" Decode.string
        |> required "line" Decode.string
        |> required "city" Decode.string
        |> required "state" Decode.string
        |> required "postalCode" Decode.string
        |> required "country" Decode.string
        |> required "period" decodePatientAddressPeriod
        |> required "preferred" Decode.string


decodePatientTelecom : Decoder PatientTelecom
decodePatientTelecom =
    Decode.succeed PatientTelecom
        |> required "use" Decode.string
        |> required "system" Decode.string
        |> required "value" Decode.string
        |> required "preferred" Decode.string


decodePatientCommunicationLanguageCoding : Decoder PatientCommunicationLanguageCoding
decodePatientCommunicationLanguageCoding =
    Decode.succeed PatientCommunicationLanguageCoding
        |> required "system" Decode.string
        |> required "code" Decode.string
        |> required "display" Decode.string


decodePatientCommunicationLanguage : Decoder PatientCommunicationLanguage
decodePatientCommunicationLanguage =
    Decode.succeed PatientCommunicationLanguage
        |> required "coding" decodePatientCommunicationLanguageCoding
        |> required "text" Decode.string


decodePatientCommunication : Decoder PatientCommunication
decodePatientCommunication =
    Decode.succeed PatientCommunication
        |> required "language" decodePatientCommunicationLanguage
        |> required "preferred" Decode.string


decodePatientManagingOrganization : Decoder PatientManagingOrganization
decodePatientManagingOrganization =
    Decode.succeed PatientManagingOrganization
        |> required "reference" Decode.string
        |> required "display" Decode.string


encodePatient : Patient -> Encode.Value
encodePatient record =
    Encode.object
        [ ( "resourceType", Encode.string <| record.resourceType )
        , ( "id", Encode.string <| record.id )
        , ( "meta", encodePatientMeta <| record.meta )
        , ( "lastModifiedBy", encodePatientLastModifiedBy <| record.lastModifiedBy )
        , ( "lastModified", Encode.string <| record.lastModified )
        , ( "multipleBirthInteger", Encode.string <| record.multipleBirthInteger )
        , ( "multipleBirthBoolean", Encode.string <| record.multipleBirthBoolean )
        , ( "identifier", encodePatientIdentifier <| record.identifier )
        , ( "name", encodePatientName <| record.name )
        , ( "gender", Encode.string <| record.gender )
        , ( "birthDate", Encode.string <| record.birthDate )
        , ( "deceasedBoolean", Encode.string <| record.deceasedBoolean )
        , ( "address", encodePatientAddress <| record.address )
        , ( "telecom", encodePatientTelecom <| record.telecom )
        , ( "communication", encodePatientCommunication <| record.communication )

        --        , ("extension",  Encode.list <| List.map encodeComplexType <| record.extension)
        , ( "managingOrganization", encodePatientManagingOrganization <| record.managingOrganization )
        , ( "active", Encode.string <| record.active )
        ]


encodePatientMeta : PatientMeta -> Encode.Value
encodePatientMeta record =
    Encode.object
        [ ( "versionId", Encode.string <| record.versionId )
        ]


encodePatientLastModifiedBy : PatientLastModifiedBy -> Encode.Value
encodePatientLastModifiedBy record =
    Encode.object
        [ ( "reference", Encode.string <| record.reference )
        , ( "display", Encode.string <| record.display )
        ]


encodePatientIdentifierAssigner : PatientIdentifierAssigner -> Encode.Value
encodePatientIdentifierAssigner record =
    Encode.object
        [ ( "reference", Encode.string <| record.reference )
        , ( "display", Encode.string <| record.display )
        ]


encodePatientIdentifier : PatientIdentifier -> Encode.Value
encodePatientIdentifier record =
    Encode.object
        [ ( "use", Encode.string <| record.use )
        , ( "type", Encode.string <| record.type_ )
        , ( "system", Encode.string <| record.system )
        , ( "value", Encode.string <| record.value )
        , ( "assigner", encodePatientIdentifierAssigner <| record.assigner )
        ]


encodePatientName : PatientName -> Encode.Value
encodePatientName record =
    Encode.object
        [ ( "use", Encode.string <| record.use )
        , ( "family", Encode.string <| record.family )
        , ( "given", Encode.string <| record.given )
        ]


encodePatientAddressPeriod : PatientAddressPeriod -> Encode.Value
encodePatientAddressPeriod record =
    Encode.object
        [ ( "start", Encode.string <| record.start )
        , ( "end", Encode.string <| record.end )
        ]


encodePatientAddress : PatientAddress -> Encode.Value
encodePatientAddress record =
    Encode.object
        [ ( "use", Encode.string <| record.use )
        , ( "line", Encode.string <| record.line )
        , ( "city", Encode.string <| record.city )
        , ( "state", Encode.string <| record.state )
        , ( "postalCode", Encode.string <| record.postalCode )
        , ( "country", Encode.string <| record.country )
        , ( "period", encodePatientAddressPeriod <| record.period )
        , ( "preferred", Encode.string <| record.preferred )
        ]


encodePatientTelecom : PatientTelecom -> Encode.Value
encodePatientTelecom record =
    Encode.object
        [ ( "use", Encode.string <| record.use )
        , ( "system", Encode.string <| record.system )
        , ( "value", Encode.string <| record.value )
        , ( "preferred", Encode.string <| record.preferred )
        ]


encodePatientCommunicationLanguageCoding : PatientCommunicationLanguageCoding -> Encode.Value
encodePatientCommunicationLanguageCoding record =
    Encode.object
        [ ( "system", Encode.string <| record.system )
        , ( "code", Encode.string <| record.code )
        , ( "display", Encode.string <| record.display )
        ]


encodePatientCommunicationLanguage : PatientCommunicationLanguage -> Encode.Value
encodePatientCommunicationLanguage record =
    Encode.object
        [ ( "coding", encodePatientCommunicationLanguageCoding <| record.coding )
        , ( "text", Encode.string <| record.text )
        ]


encodePatientCommunication : PatientCommunication -> Encode.Value
encodePatientCommunication record =
    Encode.object
        [ ( "language", encodePatientCommunicationLanguage <| record.language )
        , ( "preferred", Encode.string <| record.preferred )
        ]


encodePatientManagingOrganization : PatientManagingOrganization -> Encode.Value
encodePatientManagingOrganization record =
    Encode.object
        [ ( "reference", Encode.string <| record.reference )
        , ( "display", Encode.string <| record.display )
        ]

