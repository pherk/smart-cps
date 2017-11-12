module Views.Patient exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Data.Patient as Patient exposing (Patient)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Route exposing (Route)

-- VIEWS --


{-| Some pages want to view just the timestamp, not the whole article.
-}
viewTimestamp : Patient -> Html msg
viewTimestamp patient =
    span [ class "date" ] [ text (formattedTimestamp patient) ]


view : Patient -> Html msg
view patient =
    let
        name =
            patient.name
    in
    div [ class "article-preview" ]
        [ div [ class ""]
            [ h1 [] [ text name.family ]
            , p [] [ text patient.id ]
            , span [] [ text "Read more..." ]
            ]
        ]



-- INTERNAL --


formattedTimestamp : Patient -> String
formattedTimestamp patient =
--    Date.Format.format "%B %e, %Y" patient.lastModified
    patient.lastModified
