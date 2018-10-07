module FHIR.Resources.Comment exposing (Comment, author, body, createdAt, delete, id, list, post)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Author exposing (Author)
import CommentId exposing (CommentId)
import FHIR.Resource exposing (Resource)
import FHIR.Resources.ID as ID exposing (ID)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Time



-- TYPES


type Comment
    = Comment Internals


type alias Internals =
    { id : CommentId
    , body : String
    , createdAt : Time.Posix
    , author : Author
    }



-- INFO


id : Comment -> CommentId
id (Comment comment) =
    comment.id


body : Comment -> String
body (Comment comment) =
    comment.body


createdAt : Comment -> Time.Posix
createdAt (Comment comment) =
    comment.createdAt


author : Comment -> Author
author (Comment comment) =
    comment.author



-- LIST


list : Maybe Cred -> ID -> Http.Request (List Comment)
list maybeCred resourceID =
    Decode.field "comments" (Decode.list (decoder maybeCred))
        |> Api.get (Endpoint.comments resourceID) maybeCred



-- POST


post : ID -> String -> Cred -> Http.Request Comment
post resourceID commentBody cred =
    let
        bod =
            encodeCommentBody commentBody
                |> Http.jsonBody
    in
    Decode.field "comment" (decoder (Just cred))
        |> Api.post (Endpoint.comments resourceID) (Just cred) bod


encodeCommentBody : String -> Value
encodeCommentBody str =
    Encode.object [ ( "comment", Encode.object [ ( "body", Encode.string str ) ] ) ]



-- DELETE


delete : ID -> CommentId -> Cred -> Http.Request ()
delete resourceID commentId cred =
    Api.delete (Endpoint.comment resourceID commentId) cred Http.emptyBody (Decode.succeed ())



-- SERIALIZATION


decoder : Maybe Cred -> Decoder Comment
decoder maybeCred =
    Decode.succeed Internals
        |> required "id" CommentId.decoder
        |> required "body" Decode.string
        |> required "createdAt" Iso8601.decoder
        |> required "author" (Author.decoder maybeCred)
        |> Decode.map Comment
