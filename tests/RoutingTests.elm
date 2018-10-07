module RoutingTests exposing (..)

import FHIR.Resource as Resource exposing (Resource)
import FHIR.Resources.ID as ID exposing (ID)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeString)
import Route exposing (Route(..))
import Test exposing (..)
import Url exposing (Url)
import Username exposing (Username)


-- TODO need to add lots more tests!


fromUrl : Test
fromUrl =
    describe "Route.fromUrl"
        [ testUrl "" Home
        , testUrl "#" Root
        , testUrl "login" Login
        , testUrl "logout" Logout
        , testUrl "settings" Settings
        , testUrl "profile/foo" (Profile (usernameFromStr "foo"))
        , testUrl "register" Register
        , testUrl "resource/foo" (Resource (idFromStr "foo"))
        , testUrl "editor" NewResource
        , testUrl "editor/foo" (EditResource (idFromStr "foo"))
        ]



-- HELPERS


testUrl : String -> Route -> Test
testUrl hash route =
    test ("Parsing hash: \"" ++ hash ++ "\"") <|
        \() ->
            fragment hash
                |> Route.fromUrl
                |> Expect.equal (Just route)


fragment : String -> Url
fragment frag =
    { protocol = Url.Http
    , host = "foo.com"
    , port_ = Nothing
    , path = "bar"
    , query = Nothing
    , fragment = Just frag
    }



-- CONSTRUCTING UNEXPOSED VALUES
-- By decoding values that are not intended to be exposed directly - and erroring
-- if they cannot be decoded, since this is harmless in tests - we can let
-- our internal modules continue to expose only the intended ways of
-- constructing those, while still being able to test them.


usernameFromStr : String -> Username
usernameFromStr str =
    case decodeString Username.decoder ("\"" ++ str ++ "\"") of
        Ok username ->
            username

        Err err ->
            Debug.todo ("Error decoding Username from \"" ++ str ++ "\": " ++ Decode.errorToString err)


idFromStr : String -> ID
idFromStr str =
    let
        json =
            """
            { "description": null
            , "id": \"""" ++ str ++ """"
            , "title": ""
            , "tagList": []
            , "createdAt": "2012-04-23T18:25:43.511Z"
            , "updatedAt": "2012-04-23T18:25:43.511Z"
            , "favorited": false
            , "favoritesCount": 1
            , "author":
                 { "username": ""
                 , "bio": null
                 , "image": null
                 , "following": false
                 }
            }
        """
    in
    case decodeString (Resource.previewDecoder Nothing) json of
        Ok resource ->
            Resource.id resource

        Err err ->
            Debug.todo ("Error decoding ID from \"" ++ str ++ "\": " ++ Decode.errorToString err)

