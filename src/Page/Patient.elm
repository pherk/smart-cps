module Page.Patient exposing (Model, Msg, init, update, view)

{-| Viewing an individual patient.
-}

import Data.Patient as Patient exposing (Patient, ID)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Date exposing (Date)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Patient
import Request.Profile
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Patient
import Views.Errors
import Views.Page as Page
import Views.User.Follow as Follow


-- MODEL --


type alias Model =
    { errors : List String
    , patient : Patient
    }


init : Session -> ID -> Task PageLoadError Model
init session id =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadPatient =
            Request.Patient.get maybeAuthToken id
                |> Http.toTask

        handleLoadError e =
            let _ = Debug.log "Error:: " e
            in
            pageLoadError Page.Other  "Patient is currently unavailable."
    in
    Task.map (Model []) loadPatient 
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        patient =
            model.patient

        buttons =
            viewButtons patient session.user
    in
    div [ class "article-page" ]
        [ viewBanner model.errors patient session.user
        , div [ class "container page" ]
            [ div [ class "row article-content" ]
                [ div [ class "col-md-12" ] 
--                    [ Patient.nameToHtml patient.name [] ]
                      [   text patient.name.family ]
                ]                
            , hr [] []
            , div [ class "article-actions" ]
                  buttons
            , div [ class "row" ]
                [ div [ class "col-xs-12 col-md-8 offset-md-2" ] 
                      [ text "comments"]
                ]
            ]
        ]


viewBanner : List String -> Patient.Patient -> Maybe User -> Html Msg
viewBanner errors patient maybeUser =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [] [ text patient.name.family ]
            ]
        ]


viewButtons : Patient.Patient -> Maybe User -> List (Html Msg)
viewButtons patient maybeUser =
    let
        isAdmin =
           True 
    in
    if isAdmin then
        [ editButton patient
        , text " "
        , deleteButton patient
        ]
    else
        [ 
         text "not auth"
        ]

formatCommentTimestamp : Date -> String
formatCommentTimestamp =
    Date.Format.format "%B %e, %Y"



-- UPDATE --


type Msg
    = DismissErrors


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        patient =
            model.patient

    in
    case msg of
        DismissErrors ->
            { model | errors = [] } => Cmd.none

-- INTERNAL --

deleteButton : Patient -> Html Msg
deleteButton patient =
--  button [ class "btn btn-outline-danger btn-sm", onClick DeletePatient ]
    button [ class "btn btn-outline-danger btn-sm" ]
        [ i [ class "ion-trash-a" ] [], text " Delete Patient" ]


editButton : Patient -> Html Msg
editButton patient =
--  a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditPatient patient id) ]
    a [ class "btn btn-outline-secondary btn-sm" ]
        [ i [ class "ion-edit" ] [], text " Edit Patient" ]

