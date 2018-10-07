module Page.Resource exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| Viewing an individual resource.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import FHIR.Resource as Resource exposing (Resource, Full, Preview)
import FHIR.Resources.Body as Body exposing (Body)
import FHIR.Resources.Comment as Comment exposing (Comment)
import FHIR.Resources.ID as ID exposing (ID)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Browser.Navigation as Nav
import CommentId exposing (CommentId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Loading
import Log
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing (Username)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String

    -- Loaded independently from server
    , comments : Status ( CommentText, List Comment )
    , resource : Status (Resource Full)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type CommentText
    = Editing String
    | Sending String


init : Session -> ID -> ( Model, Cmd Msg )
init session id =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , comments = Loading
      , resource = Loading
      }
    , Cmd.batch
        [ Resource.fetch maybeCred id
            |> Http.send CompletedLoadResource
        , Comment.list maybeCred id
            |> Http.send CompletedLoadComments
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.resource of
        Loaded resource ->
            let
                { title } =
                    Resource.metadata resource

                author =
                    Resource.author resource

                avatar =
                    Profile.avatar (Author.profile author)

                id =
                    Resource.id resource

                profile =
                    Author.profile author

                buttons =
                    case Session.cred model.session of
                        Just cred ->
                            viewButtons cred resource author

                        Nothing ->
                            []
            in
            { title = title
            , content =
                div [ class "resource-page" ]
                    [ div [ class "banner" ]
                        [ div [ class "container" ]
                            [ h1 [] [ text title ]
                            , div [ class "resource-meta" ] <|
                                List.append
                                    [ a [ Route.href (Route.Profile (Author.username author)) ]
                                        [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                                    , div [ class "info" ]
                                        [ Author.view (Author.username author)
                                        , Timestamp.view model.timeZone (Resource.metadata resource).createdAt
                                        ]
                                    ]
                                    buttons
                            , Page.viewErrors ClickedDismissErrors model.errors
                            ]
                        ]
                    , div [ class "container page" ]
                        [ div [ class "row resource-content" ]
                            [ div [ class "col-md-12" ]
                                [ Body.toHtml (Resource.body resource) [] ]
                            ]
                        , hr [] []
                        , div [ class "resource-actions" ]
                            [ div [ class "resource-meta" ] <|
                                List.append
                                    [ a [ Route.href (Route.Profile (Author.username author)) ]
                                        [ img [ Avatar.src avatar ] [] ]
                                    , div [ class "info" ]
                                        [ Author.view (Author.username author)
                                        , Timestamp.view model.timeZone (Resource.metadata resource).createdAt
                                        ]
                                    ]
                                    buttons
                            ]
                        , div [ class "row" ]
                            [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                                -- Don't render the comments until the resource has loaded!
                                case model.comments of
                                    Loading ->
                                        []

                                    LoadingSlowly ->
                                        [ Loading.icon ]

                                    Loaded ( commentText, comments ) ->
                                        -- Don't let users add comments until they can
                                        -- see the existing comments! Otherwise you
                                        -- may be about to repeat something that's
                                        -- already been said.
                                        viewAddComment id commentText (Session.viewer model.session)
                                            :: List.map (viewComment model.timeZone id) comments

                                    Failed ->
                                        [ Loading.error "comments" ]
                            ]
                        ]
                    ]
            }

        Loading ->
            { title = "Resource", content = text "" }

        LoadingSlowly ->
            { title = "Resource", content = Loading.icon }

        Failed ->
            { title = "Resource", content = Loading.error "resource" }


viewAddComment : ID -> CommentText -> Maybe Viewer -> Html Msg
viewAddComment id commentText maybeViewer =
    case maybeViewer of
        Just viewer ->
            let
                avatar =
                    Viewer.avatar viewer

                cred =
                    Viewer.cred viewer

                ( commentStr, buttonAttrs ) =
                    case commentText of
                        Editing str ->
                            ( str, [] )

                        Sending str ->
                            ( str, [ disabled True ] )
            in
            Html.form [ class "card comment-form", onSubmit (ClickedPostComment cred id) ]
                [ div [ class "card-block" ]
                    [ textarea
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        , attribute "rows" "3"
                        , onInput EnteredCommentText
                        , value commentStr
                        ]
                        []
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", Avatar.src avatar ] []
                    , button
                        (class "btn btn-sm btn-primary" :: buttonAttrs)
                        [ text "Post Comment" ]
                    ]
                ]

        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to comment."
                ]


viewButtons : Cred -> Resource Full -> Author -> List (Html Msg)
viewButtons cred resource author =
    case author of
        IsFollowing followedAuthor ->
            [ Author.unfollowButton ClickedUnfollow cred followedAuthor
            , text " "
            , favoriteButton cred resource
            ]

        IsNotFollowing unfollowedAuthor ->
            [ Author.followButton ClickedFollow cred unfollowedAuthor
            , text " "
            , favoriteButton cred resource
            ]

        IsViewer _ _ ->
            [ editButton resource
            , text " "
            , deleteButton cred resource
            ]


viewComment : Time.Zone -> ID -> Comment -> Html Msg
viewComment timeZone id comment =
    let
        author =
            Comment.author comment

        profile =
            Author.profile author

        authorUsername =
            Author.username author

        deleteCommentButton =
            case author of
                IsViewer cred _ ->
                    let
                        msg =
                            ClickedDeleteComment cred id (Comment.id comment)
                    in
                    span
                        [ class "mod-options"
                        , onClick msg
                        ]
                        [ i [ class "ion-trash-a" ] [] ]

                _ ->
                    -- You can't delete other peoples' comments!
                    text ""

        timestamp =
            Timestamp.format timeZone (Comment.createdAt comment)
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text (Comment.body comment) ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img [ class "comment-author-img", Avatar.src (Profile.avatar profile) ] []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile authorUsername) ]
                [ text (Username.toString authorUsername) ]
            , span [ class "date-posted" ] [ text timestamp ]
            , deleteCommentButton
            ]
        ]



-- UPDATE


type Msg
    = ClickedDeleteResource Cred ID
    | ClickedDeleteComment Cred ID CommentId
    | ClickedDismissErrors
    | ClickedFavorite Cred ID Body
    | ClickedUnfavorite Cred ID Body
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedPostComment Cred ID
    | EnteredCommentText String
    | CompletedLoadResource (Result Http.Error (Resource Full))
    | CompletedLoadComments (Result Http.Error (List Comment))
    | CompletedDeleteResource (Result Http.Error ())
    | CompletedDeleteComment CommentId (Result Http.Error ())
    | CompletedFavoriteChange (Result Http.Error (Resource Full))
    | CompletedFollowChange (Result Http.Error Author)
    | CompletedPostComment (Result Http.Error Comment)
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedFavorite cred id body ->
            ( model, fave Resource.favorite cred id body )

        ClickedUnfavorite cred id body ->
            ( model, fave Resource.unfavorite cred id body )

        CompletedLoadResource (Ok resource) ->
            ( { model | resource = Loaded resource }, Cmd.none )

        CompletedLoadResource (Err error) ->
            ( { model | resource = Failed }
            , Log.error
            )

        CompletedLoadComments (Ok comments) ->
            ( { model | comments = Loaded ( Editing "", comments ) }, Cmd.none )

        CompletedLoadComments (Err error) ->
            ( { model | resource = Failed }, Log.error )

        CompletedFavoriteChange (Ok newResource) ->
            ( { model | resource = Loaded newResource }, Cmd.none )

        CompletedFavoriteChange (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Author.requestUnfollow followedAuthor cred
                |> Http.send CompletedFollowChange
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Author.requestFollow unfollowedAuthor cred
                |> Http.send CompletedFollowChange
            )

        CompletedFollowChange (Ok newAuthor) ->
            case model.resource of
                Loaded resource ->
                    ( { model | resource = Loaded (Resource.mapAuthor (\_ -> newAuthor) resource) }, Cmd.none )

                _ ->
                    ( model, Log.error )

        CompletedFollowChange (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        EnteredCommentText str ->
            case model.comments of
                Loaded ( Editing _, comments ) ->
                    -- You can only edit comment text once comments have loaded
                    -- successfully, and when the comment is not currently
                    -- being submitted.
                    ( { model | comments = Loaded ( Editing str, comments ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        ClickedPostComment cred id ->
            case model.comments of
                Loaded ( Editing "", comments ) ->
                    -- No posting empty comments!
                    -- We don't use Log.error here because this isn't an error,
                    -- it just doesn't do anything.
                    ( model, Cmd.none )

                Loaded ( Editing str, comments ) ->
                    ( { model | comments = Loaded ( Sending str, comments ) }
                    , cred
                        |> Comment.post id str
                        |> Http.send CompletedPostComment
                    )

                _ ->
                    -- Either we have no comment to post, or there's already
                    -- one in the process of being posted, or we don't have
                    -- a valid resource, in which case how did we post this?
                    ( model, Log.error )

        CompletedPostComment (Ok comment) ->
            case model.comments of
                Loaded ( _, comments ) ->
                    ( { model | comments = Loaded ( Editing "", comment :: comments ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        CompletedPostComment (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedDeleteComment cred id cid ->
            ( model
            , cred
                |> Comment.delete id cid
                |> Http.send (CompletedDeleteComment cid)
            )

        CompletedDeleteComment id (Ok ()) ->
            case model.comments of
                Loaded ( commentText, comments ) ->
                    ( { model | comments = Loaded ( commentText, withoutComment id comments ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        CompletedDeleteComment id (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedDeleteResource cred id ->
            ( model
            , delete id cred
                |> Http.send CompletedDeleteResource
            )

        CompletedDeleteResource (Ok ()) ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Home )

        CompletedDeleteResource (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                resource =
                    case model.resource of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                comments =
                    case model.comments of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | resource = resource, comments = comments }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


delete : ID -> Cred -> Http.Request ()
delete id cred =
    Api.delete (Endpoint.resource id) cred Http.emptyBody (Decode.succeed ())



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


fave : (ID -> Cred -> Http.Request (Resource Preview)) -> Cred -> ID -> Body -> Cmd Msg
fave toRequest cred id body =
    toRequest id cred
        |> Http.toTask
        |> Task.map (Resource.fromPreview body)
        |> Task.attempt CompletedFavoriteChange


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id list =
    List.filter (\comment -> Comment.id comment /= id) list


favoriteButton : Cred -> Resource Full -> Html Msg
favoriteButton cred resource =
    let
        { favoritesCount, favorited } =
            Resource.metadata resource

        id =
            Resource.id resource

        body =
            Resource.body resource

        kids =
            [ text (" Favorite Resource (" ++ String.fromInt favoritesCount ++ ")") ]
    in
    if favorited then
        Resource.unfavoriteButton cred (ClickedUnfavorite cred id body) [] kids

    else
        Resource.favoriteButton cred (ClickedFavorite cred id body) [] kids


deleteButton : Cred -> Resource a -> Html Msg
deleteButton cred resource =
    let
        msg =
            ClickedDeleteResource cred (Resource.id resource)
    in
    button [ class "btn btn-outline-danger btn-sm", onClick msg ]
        [ i [ class "ion-trash-a" ] [], text " Delete Resource" ]


editButton : Resource a -> Html Msg
editButton resource =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditResource (Resource.id resource)) ]
        [ i [ class "ion-edit" ] [], text " Edit Resource" ]


