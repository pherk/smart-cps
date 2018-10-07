module Route exposing (Route(..), fromUrl, href, replaceUrl)

import FHIR.Resources.ID as ID exposing (ID)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Profile exposing (Profile)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)

-- ROUTING --


type Route
    = Home
    | Root
    | Login
    | Logout
    | Register
    | Settings
    | Resource ID
    | Profile Username
    | NewResource
    | EditResource ID


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home (s "")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> Username.urlParser)
        , Parser.map Register (s "register")
        , Parser.map Resource (s "resource" </> ID.urlParser)
        , Parser.map NewResource (s "editor")
        , Parser.map EditResource (s "editor" </> ID.urlParser)
        ]


-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


-- INTERNAL 


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Resource id ->
                    [ "resource", ID.toString id ]

                Profile username ->
                    [ "profile", Username.toString username ]

                NewResource ->
                    [ "editor" ]

                EditResource id ->
                    [ "editor", ID.toString id ]
    in
    "#/" ++ String.join "/" pieces


