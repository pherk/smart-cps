module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Patient as Patient exposing (Tag)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Patient
import SelectList exposing (SelectList)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)
import Views.Page as Page


-- MODEL --


type alias Model =
    { tags : List Tag
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadPatients =
            Request.Patient.tags
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
    Task.map Model loadPatients
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ 
                  div [ class "col-md-3" ]
                    [ div [ class "sidebar" ]
                        [ p [] [ text "Popular Tags" ]
                        , viewTags model.tags
                        ]
                    ]
                ]
            ]
        ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "Smart-CarePlans" ]
            , p [] [ text "A place to review FHIR workflows." ]
            ]
        ]


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        , onClick (SelectTag tagName)
        ]
        [ text (Patient.tagToString tagName) ]



-- UPDATE --


type Msg
    = SelectTag Tag


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SelectTag tagName ->
            (model, Cmd.none) 
