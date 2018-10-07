module FHIR.Resource exposing
    ( 
      Resource
    , Full
    , Preview
    , author
    , body
    , favorite, favoriteButton
    , fetch
    , fromPreview
    , fullDecoder
    , id
    , mapAuthor
    , metadata
    , previewDecoder
    , unfavorite, unfavoriteButton
    )


import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Author exposing (Author)
import FHIR.Resources.Body as Body exposing (Body)
import FHIR.Resources.ID as ID exposing (ID)
import FHIR.Resources.Patient as Patient exposing (Patient)
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Markdown
import Profile exposing (Profile)
import Time
import Username as Username exposing (Username)
import Viewer exposing (Viewer)

-- TYPES


{-| An FHIR resource, optionally with resource body.
-}

type Resource a
    = Resource Internals a


{-| Metadata about the resource - its title, description, and so on.

Importantly, this module's public API exposes a way to read this metadata, but
not to alter it. This is read-only information!

If we find ourselves using any particular piece of metadata often,
for example `title`, we could expose a convenience function like this:

Article.title : Article a -> String

If you like, it's totally reasonable to expose a function like that for every one
of these fields!

(Okay, to be completely honest, exposing one function per field is how I prefer
to do it, and that's how I originally wrote this module. However, I'm aware that
this code base has become a common reference point for beginners, and I think it
is _extremely important_ that slapping some "getters and setters" on a record
does not become a habit for anyone who is getting started with Elm. The whole
point of making the Article type opaque is to create guarantees through
_selectively choosing boundaries_ around it. If you aren't selective about
where those boundaries are, and instead expose a "getter and setter" for every
field in the record, the result is an API with no more guarantees than if you'd
exposed the entire record directly! It is so important to me that beginners not
fall into the terrible "getters and setters" trap that I've exposed this
Metadata record instead of exposing a single function for each of its fields,
as I did originally. This record is not a bad way to do it, by any means,
but if this seems at odds with <https://youtu.be/x1FU3e0sT1I> - now you know why!
)

-}
type alias Metadata =
    { description : String
    , title : String
    , tags : List String
    , createdAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
--    , lastModified : Time.Posix
    }


type alias Internals =
    { id : ID
    , author : Author
    , metadata : Metadata
    }


type Full
    = Full Body

type Preview
    = Preview


-- INFO

author : Resource a -> Author
author (Resource internals _) =
    internals.author


metadata : Resource a -> Metadata
metadata (Resource internals _) =
    internals.metadata


id : Resource a -> ID
id (Resource internals _) =
    internals.id


body : Resource Full -> Body
body (Resource _ (Full extraInfo)) =
    extraInfo



-- TRANSFORM


{-| This is the only way you can transform an existing article:
you can change its author (e.g. to follow or unfollow them).
All other article data necessarily comes from the server!

We can tell this for sure by looking at the types of the exposed functions
in this module.

-}

mapAuthor : (Author -> Author) -> Resource a -> Resource a
mapAuthor transform (Resource info extras) =
    Resource { info | author = transform info.author } extras

fromPreview : Body -> Resource Preview -> Resource Full
fromPreview newBody (Resource info Preview) =
    Resource info (Full newBody)



-- SERIALIZATION


previewDecoder : Maybe Cred -> Decoder (Resource Preview)
previewDecoder maybeCred =
    Decode.succeed Resource
        |> custom (internalsDecoder maybeCred)
        |> hardcoded Preview


fullDecoder : Maybe Cred -> Decoder (Resource Full)
fullDecoder maybeCred =
    Decode.succeed Resource
        |> custom (internalsDecoder maybeCred)
        |> required "body" (Decode.map Full Body.decoder)


internalsDecoder : Maybe Cred -> Decoder Internals
internalsDecoder maybeCred =
    Decode.succeed Internals
        |> required "id" ID.decoder
        |> required "author" (Author.decoder maybeCred)
        |> custom metadataDecoder


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Iso8601.decoder
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
--        |> required "lastModified" Iso8601.decoder



-- SINGLE


fetch : Maybe Cred -> ID -> Http.Request (Resource Full)
fetch maybeCred resourceID =
    Decode.field "resource" (fullDecoder maybeCred)
        |> Api.get (Endpoint.resource resourceID) maybeCred

-- FAVORITE


favorite : ID -> Cred -> Http.Request (Resource Preview)
favorite resourceID cred =
    Api.post (Endpoint.favorite resourceID) (Just cred) Http.emptyBody (faveDecoder cred)


unfavorite : ID -> Cred -> Http.Request (Resource Preview)
unfavorite resourceID cred =
    Api.delete (Endpoint.favorite resourceID) cred Http.emptyBody (faveDecoder cred)


faveDecoder : Cred -> Decoder (Resource Preview)
faveDecoder cred =
    Decode.field "resource" (previewDecoder (Just cred))


{-| This is a "build your own element" API.
You pass it some configuration, followed by a `List (Attribute msg)` and a
`List (Html msg)`, just like any standard Html element.
-}
favoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
favoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-outline-primary" msg attrs kids


unfavoriteButton :
    Cred
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
unfavoriteButton _ msg attrs kids =
    toggleFavoriteButton "btn btn-sm btn-primary" msg attrs kids


toggleFavoriteButton :
    String
    -> msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
toggleFavoriteButton classStr msg attrs kids =
    Html.button
        (class classStr :: onClickStopPropagation msg :: attrs)
        (i [ class "ion-heart" ] [] :: kids)


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))

