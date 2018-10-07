module FHIR.Resources.Feed exposing (Model, Msg, decoder, init, update, viewResources, viewPagination, viewTabs)

import Api exposing (Cred)
import FHIR.Resource as Resource exposing (Resource, Full, Preview)
import FHIR.Resources.ID as ResourceID exposing (ID)
import FHIR.Resources.Tag as Tag exposing (Tag)
import Author
import Avatar exposing (Avatar)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Page
import PaginatedList exposing (PaginatedList)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Url exposing (Url)
import Username exposing (Username)


{-| NOTE: This module has its own Model, view, and update. This is not normal!
If you find yourself doing this often, please watch <https://www.youtube.com/watch?v=DoA4Txr4GUs>

This is the reusable Resource Feed that appears on both the Home page as well as
on the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}



-- MODEL


type Model
    = Model Internals


{-| This should not be exposed! We want to benefit from the guarantee that only
this module can create or alter this model. This way if it ever ends up in
a surprising state, we know exactly where to look: this module.
-}
type alias Internals =
    { session : Session
    , errors : List String
    , resources : PaginatedList (Resource Preview)
    , isLoading : Bool
    }


init : Session -> PaginatedList (Resource Preview) -> Model
init session resources =
    Model
        { session = session
        , errors = []
        , resources = resources
        , isLoading = False
        }



-- VIEW


viewResources : Time.Zone -> Model -> List (Html Msg)
viewResources timeZone (Model { resources, session, errors }) =
    let
        maybeCred =
            Session.cred session

        resourcesHtml =
            PaginatedList.values resources
                |> List.map (viewPreview maybeCred timeZone)
    in
    Page.viewErrors ClickedDismissErrors errors :: resourcesHtml


viewPreview : Maybe Cred -> Time.Zone -> Resource Preview -> Html Msg
viewPreview maybeCred timeZone resource =
    let
        id =
            Resource.id resource

        { title, description, createdAt } =
            Resource.metadata resource

        author =
            Resource.author resource

        profile =
            Author.profile author

        username =
            Author.username author

        faveButton =
            case maybeCred of
                Just cred ->
                    let
                        { favoritesCount, favorited } =
                            Resource.metadata resource

                        viewButton =
                            if favorited then
                                Resource.unfavoriteButton cred (ClickedUnfavorite cred id)

                            else
                                Resource.favoriteButton cred (ClickedFavorite cred id)
                    in
                    viewButton [ class "pull-xs-right" ]
                        [ text (" " ++ String.fromInt favoritesCount) ]

                Nothing ->
                    text ""

    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile username) ]
                [ img [ Avatar.src (Profile.avatar profile) ] [] ]
            , div [ class "info" ]
                [ Author.view username
                , Timestamp.view timeZone createdAt
                ]
            , faveButton
            ]
        , a [ class "preview-link", Route.href (Route.Resource (Resource.id resource)) ]
            [ h1 [] [ text title ]
            , p [] [ text description ]
            , span [] [ text "Read more..." ]
            , ul [ class "tag-list" ]
                (List.map viewTag (Resource.metadata resource).tags)
            ]
        ]


viewTabs :
    List ( String, msg )
    -> ( String, msg )
    -> List ( String, msg )
    -> Html msg
viewTabs before selected after =
    ul [ class "nav nav-pills outline-active" ] <|
        List.concat
            [ List.map (viewTab []) before
            , [ viewTab [ class "active" ] selected ]
            , List.map (viewTab []) after
            ]


viewTab : List (Attribute msg) -> ( String, msg ) -> Html msg
viewTab attrs ( name, msg ) =
    li [ class "nav-item" ]
        [ -- Note: The RealWorld CSS requires an href to work properly.
          a (class "nav-link" :: onClick msg :: href "" :: attrs)
            [ text name ]
        ]


viewPagination : (Int -> msg) -> Int -> Model -> Html msg
viewPagination toMsg page (Model feed) =
    let
        viewPageLink currentPage =
            pageLink toMsg currentPage (currentPage == page)

        totalPages =
            PaginatedList.total feed.resources
    in
    if totalPages > 1 then
        List.range 1 totalPages
            |> List.map viewPageLink
            |> ul [ class "pagination" ]

    else
        Html.text ""


pageLink : (Int -> msg) -> Int -> Bool -> Html msg
pageLink toMsg targetPage isActive =
    li [ classList [ ( "page-item", True ), ( "active", isActive ) ] ]
        [ a
            [ class "page-link"
            , onClick (toMsg targetPage)

            -- The RealWorld CSS requires an href to work properly.
            , href ""
            ]
            [ text (String.fromInt targetPage) ]
        ]


viewTag : String -> Html msg
viewTag tagName =
    li [ class "tag-default tag-pill tag-outline" ] [ text tagName ]



-- UPDATE


type Msg
    = ClickedDismissErrors
    | ClickedFavorite Cred ID
    | ClickedUnfavorite Cred ID
    | CompletedFavorite (Result Http.Error (Resource Preview))


update : Maybe Cred -> Msg -> Model -> ( Model, Cmd Msg )
update maybeCred msg (Model model) =
    case msg of
        ClickedDismissErrors ->
            ( Model { model | errors = [] }, Cmd.none )

        ClickedFavorite cred id ->
            fave Resource.favorite cred id model

        ClickedUnfavorite cred id ->
            fave Resource.unfavorite cred id model

        CompletedFavorite (Ok resource) ->
            ( Model { model | resources = PaginatedList.map (replaceResource resource) model.resources }
            , Cmd.none
            )

        CompletedFavorite (Err error) ->
            ( Model { model | errors = Api.addServerError model.errors }
            , Cmd.none
            )


replaceResource : Resource a -> Resource a -> Resource a
replaceResource newResource oldResource =
    if Resource.id newResource == Resource.id oldResource then
        newResource

    else
        oldResource



-- SERIALIZATION


decoder : Maybe Cred -> Int -> Decoder (PaginatedList (Resource Preview))
decoder maybeCred resultsPerPage =
    Decode.succeed PaginatedList.fromList
        |> required "resourcesCount" (pageCountDecoder resultsPerPage)
        |> required "resources" (Decode.list (Resource.previewDecoder maybeCred))


pageCountDecoder : Int -> Decoder Int
pageCountDecoder resultsPerPage =
    Decode.int
        |> Decode.map (\total -> ceiling (toFloat total / toFloat resultsPerPage))



-- INTERNAL


fave : (ID -> Cred -> Http.Request (Resource Preview)) -> Cred -> ID -> Internals -> ( Model, Cmd Msg )
fave toRequest cred id model =
    ( Model model
    , toRequest id cred
        |> Http.toTask
        |> Task.attempt CompletedFavorite
    )

