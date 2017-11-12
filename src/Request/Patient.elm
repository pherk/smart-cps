module Request.Patient
    exposing
        ( 
          ListConfig
        , get
--        , list
        , tags
        , defaultListConfig
        )

import Data.Patient as Patient exposing (Patient, ID, Tag, PatientName, idToString)
import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.User as User exposing (Username)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withHeader, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Request.Helpers exposing (apiUrl)
import Util exposing ((=>))
import Debug

-- SINGLE --


get : Maybe AuthToken -> Patient.ID -> Http.Request (Patient)
get maybeToken id =
    let
        expect =
            Patient.decodePatient
--              |> Decode.field "resourceType"
                |> Http.expectJson
        _ = Debug.log "Patient: " expect
    in
    ("http://localhost:8080/exist/restxq/nabu/patients/" ++ Patient.idToString id)
        |> HttpBuilder.get
        |> withHeader "Content-Type" "application/json"
        |> HttpBuilder.withExpect expect
--      |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- LIST --


type alias ListConfig =
    { tag : Maybe Tag
    , author : Maybe Username
    , favorited : Maybe Username
    , limit : Int
    , offset : Int
    }


defaultListConfig : ListConfig
defaultListConfig =
    { tag = Nothing
    , author = Nothing
    , favorited = Nothing
    , limit = 20
    , offset = 0
    }

-- TAGS --


tags : Http.Request (List Tag)
tags =
    Decode.field "tags" (Decode.list Patient.tagDecoder)
        |> Http.get (apiUrl "/tags")



-- HELPERS --


maybeVal : ( a, Maybe b ) -> Maybe ( a, b )
maybeVal ( key, value ) =
    case value of
        Nothing ->
            Nothing

        Just val ->
            Just (key => val)

