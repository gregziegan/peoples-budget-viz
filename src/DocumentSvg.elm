module DocumentSvg exposing (view)

import Color exposing (Color)
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Metadata exposing (Metadata)
import Pages
import Pages.ImagePath as ImagePath
import Pages.PagePath as PagePath exposing (PagePath)
import Palette
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Element msg
view =
    Element.image
        [ Element.width (Element.px 150)
        , Font.color Palette.color.primary
        ]
        { src = ImagePath.toString Pages.images.author.peoplesBudgetLogo, description = "Nashville People's Budget" }
