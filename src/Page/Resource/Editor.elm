module Page.Resource.Editor exposing (Model, Msg, initEdit, initNew, subscriptions, toSession, update, view)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import FHIR.Resource as Resource exposing (Resource, Full)
import FHIR.Resources.Body as Body exposing (Body)
import FHIR.Resources.ID as ID exposing (ID)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Loading
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time



-- MODEL


type alias Model =
    { session : Session
    , status : Status
    }


type
    Status
    -- Edit Resource
    = Loading ID
    | LoadingSlowly ID
    | LoadingFailed ID
    | Saving ID Form
    | Editing ID (List Problem) Form
      -- New Resource
    | EditingNew (List Problem) Form
    | Creating Form


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { title : String
    , body : String
    , description : String
    , tags : String
    }


initNew : Session -> ( Model, Cmd msg )
initNew session =
    ( { session = session
      , status =
            EditingNew []
                { title = ""
                , body = ""
                , description = ""
                , tags = ""
                }
      }
    , Cmd.none
    )


initEdit : Session -> ID -> ( Model, Cmd Msg )
initEdit session id =
    ( { session = session
      , status = Loading id
      }
    , Cmd.batch
        [ Resource.fetch (Session.cred session) id
            |> Http.toTask
            -- If init fails, store the id that failed in the msg, so we can
            -- at least have it later to display the page's title properly!
            |> Task.mapError (\httpError -> ( id, httpError ))
            |> Task.attempt CompletedResourceLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title =
        case getID model.status of
            Just id ->
                "Edit Resource - " ++ ID.toString id

            Nothing ->
                "New Resource"
    , content =
        case Session.cred model.session of
            Just cred ->
                viewAuthenticated cred model

            Nothing ->
                text "Sign in to edit this resource."
    }


viewProblems : List Problem -> Html msg
viewProblems problems =
    ul [ class "error-messages" ]
        (List.map viewProblem problems)


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ message ->
                    message

                ServerError message ->
                    message
    in
    li [] [ text errorMessage ]


viewAuthenticated : Cred -> Model -> Html Msg
viewAuthenticated cred model =
    let
        formHtml =
            case model.status of
                Loading _ ->
                    []

                LoadingSlowly _ ->
                    [ Loading.icon ]

                Saving id form ->
                    [ viewForm cred form (editResourceSaveButton [ disabled True ]) ]

                Creating form ->
                    [ viewForm cred form (newResourceSaveButton [ disabled True ]) ]

                Editing id problems form ->
                    [ viewProblems problems
                    , viewForm cred form (editResourceSaveButton [])
                    ]

                EditingNew problems form ->
                    [ viewProblems problems
                    , viewForm cred form (newResourceSaveButton [])
                    ]

                LoadingFailed _ ->
                    [ text "Resource failed to load." ]
    in
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    formHtml
                ]
            ]
        ]


viewForm : Cred -> Form -> Html Msg -> Html Msg
viewForm cred fields saveButton =
    Html.form [ onSubmit (ClickedSave cred) ]
        [ fieldset []
            [ fieldset [ class "form-group" ]
                [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Resource Title"
                    , onInput EnteredTitle
                    , value fields.title
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "What's this resource about?"
                    , onInput EnteredDescription
                    , value fields.description
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ textarea
                    [ class "form-control"
                    , placeholder "Write your resource (in markdown)"
                    , attribute "rows" "8"
                    , onInput EnteredBody
                    , value fields.body
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Enter tags"
                    , onInput EnteredTags
                    , value fields.tags
                    ]
                    []
                ]
            , saveButton
            ]
        ]


editResourceSaveButton : List (Attribute msg) -> Html msg
editResourceSaveButton extraAttrs =
    saveResourceButton "Update Resource" extraAttrs


newResourceSaveButton : List (Attribute msg) -> Html msg
newResourceSaveButton extraAttrs =
    saveResourceButton "Publish Resource" extraAttrs


saveResourceButton : String -> List (Attribute msg) -> Html msg
saveResourceButton caption extraAttrs =
    button (class "btn btn-lg pull-xs-right btn-primary" :: extraAttrs)
        [ text caption ]



-- UPDATE


type Msg
    = ClickedSave Cred
    | EnteredBody String
    | EnteredDescription String
    | EnteredTags String
    | EnteredTitle String
    | CompletedCreate (Result Http.Error (Resource Full))
    | CompletedEdit (Result Http.Error (Resource Full))
    | CompletedResourceLoad (Result ( ID, Http.Error ) (Resource Full))
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave cred ->
            model.status
                |> save cred
                |> Tuple.mapFirst (\status -> { model | status = status })

        EnteredTitle title ->
            updateForm (\form -> { form | title = title }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        EnteredTags tags ->
            updateForm (\form -> { form | tags = tags }) model

        EnteredBody body ->
            updateForm (\form -> { form | body = body }) model

        CompletedCreate (Ok resource) ->
            ( model
            , Route.Resource (Resource.id resource)
                |> Route.replaceUrl (Session.navKey model.session)
            )

        CompletedCreate (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

        CompletedEdit (Ok resource) ->
            ( model
            , Route.Resource (Resource.id resource)
                |> Route.replaceUrl (Session.navKey model.session)
            )

        CompletedEdit (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

        CompletedResourceLoad (Err ( id, error )) ->
            ( { model | status = LoadingFailed id }
            , Cmd.none
            )

        CompletedResourceLoad (Ok resource) ->
            let
                { title, description, tags } =
                    Resource.metadata resource

                status =
                    Editing (Resource.id resource)
                        []
                        { title = title
                        , body = Body.toMarkdownString (Resource.body resource)
                        , description = description
                        , tags = String.join " " tags
                        }
            in
            ( { model | status = status }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                status =
                    case model.status of
                        Loading id ->
                            LoadingSlowly id

                        other ->
                            other
            in
            ( { model | status = status }, Cmd.none )


save : Cred -> Status -> ( Status, Cmd Msg )
save cred status =
    case status of
        Editing id _ form ->
            case validate form of
                Ok validForm ->
                    ( Saving id form
                    , edit id validForm cred
                        |> Http.send CompletedEdit
                    )

                Err problems ->
                    ( Editing id problems form
                    , Cmd.none
                    )

        EditingNew _ form ->
            case validate form of
                Ok validForm ->
                    ( Creating form
                    , create validForm cred
                        |> Http.send CompletedCreate
                    )

                Err problems ->
                    ( EditingNew problems form
                    , Cmd.none
                    )

        _ ->
            -- We're in a state where saving is not allowed.
            -- We tried to prevent getting here by disabling the Save
            -- button, but somehow the user got here anyway!
            --
            -- If we had an error logging service, we would send
            -- something to it here!
            ( status, Cmd.none )


savingError : Http.Error -> Status -> Status
savingError error status =
    let
        problems =
            [ ServerError "Error saving resource" ]
    in
    case status of
        Saving id form ->
            Editing id problems form

        Creating form ->
            EditingNew problems form

        _ ->
            status


{-| Helper function for `update`. Updates the form, if there is one,
and returns Cmd.none.
Useful for recording form fields!
This could also log errors to the server if we are trying to record things in
the form and we don't actually have a form.
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    let
        newModel =
            case model.status of
                Loading _ ->
                    model

                LoadingSlowly _ ->
                    model

                LoadingFailed _ ->
                    model

                Saving id form ->
                    { model | status = Saving id (transform form) }

                Editing id errors form ->
                    { model | status = Editing id errors (transform form) }

                EditingNew errors form ->
                    { model | status = EditingNew errors (transform form) }

                Creating form ->
                    { model | status = Creating (transform form) }
    in
    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Title
    | Body


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Title
    , Body
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Title ->
                if String.isEmpty form.title then
                    [ "title can't be blank." ]

                else
                    []

            Body ->
                if String.isEmpty form.body then
                    [ "body can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { title = String.trim form.title
        , body = String.trim form.body
        , description = String.trim form.description
        , tags = String.trim form.tags
        }



-- HTTP


create : TrimmedForm -> Cred -> Http.Request (Resource Full)
create (Trimmed form) cred =
    let
        resource =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                , ( "tagList", Encode.list Encode.string (tagsFromString form.tags) )
                ]

        body =
            Encode.object [ ( "resource", resource ) ]
                |> Http.jsonBody
    in
    Decode.field "resource" (Resource.fullDecoder (Just cred))
        |> Api.post (Endpoint.resources []) (Just cred) body


tagsFromString : String -> List String
tagsFromString str =
    String.split " " str
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


edit : ID -> TrimmedForm -> Cred -> Http.Request (Resource Full)
edit resourceID (Trimmed form) cred =
    let
        resource =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                ]

        body =
            Encode.object [ ( "resource", resource ) ]
                |> Http.jsonBody
    in
    Decode.field "resource" (Resource.fullDecoder (Just cred))
        |> Api.put (Endpoint.resource resourceID) cred body



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


{-| Used for setting the page's title.
-}
getID : Status -> Maybe ID
getID status =
    case status of
        Loading id ->
            Just id

        LoadingSlowly id ->
            Just id

        LoadingFailed id ->
            Just id

        Saving id _ ->
            Just id

        Editing id _ _ ->
            Just id

        EditingNew _ _ ->
            Nothing

        Creating _ ->
            Nothing
