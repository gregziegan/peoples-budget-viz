module Data.Author exposing (Author, all, decoder, view)

import Element exposing (Element)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)


type alias Author =
    { name : String
    , avatar : ImagePath Pages.PathKey
    , bio : String
    }


all : List Author
all =
    [ { name = "People's Budget Nashville"
      , avatar = Pages.images.author.peoplesBudgetLogo
      , bio = "The Nashville People’s Budget Coalition is building a Nashville where public safety includes communities with fully funded education, access to housing and health care, and freedom from policing and jails. To make this future real, we are raising public awareness about the Metro Nashville city budget process and organizing community members to create a people’s budget that invests in the wellbeing, health, and freedom of every community member. Our coalition encompasses organizations fighting for fair wages and safe, dignified workplaces, divestment from policing and jails, investment in restorative practices, and access to housing and transportation. We know that Nashville can be a city where every community member has access to the tools and resources they need to thrive if we are committed to investing public dollars into social goods and divesting from social control."
      }
    ]


decoder : Decoder Author
decoder =
    Decode.string
        |> Decode.andThen
            (\lookupName ->
                case List.Extra.find (\currentAuthor -> currentAuthor.name == lookupName) all of
                    Just author ->
                        Decode.succeed author

                    Nothing ->
                        Decode.fail ("Couldn't find author with name " ++ lookupName ++ ". Options are " ++ String.join ", " (List.map .name all))
            )


view : List (Element.Attribute msg) -> Author -> Element msg
view attributes author =
    Element.image
        (Element.width (Element.px 70)
            :: Element.htmlAttribute (Attr.class "avatar")
            :: attributes
        )
        { src = ImagePath.toString author.avatar, description = author.name }
