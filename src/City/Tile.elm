module City.Tile exposing (Tile, attrs, blank, default)

import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)


type alias Tile =
    { width : Int
    , height : Int
    , transform : String
    }


default : Tile
default =
    { height = 200
    , width = 200
    , transform = ""
    }


attrs : Tile -> List (Svg.Attribute msg)
attrs tile =
    [ Attr.width (String.fromInt tile.width)
    , Attr.height (String.fromInt tile.height)
    , transform tile.transform
    ]


blank : Tile -> Svg msg
blank tile =
    svg (attrs tile ++ [ viewBox "0 0 65.126 40.952" ])
        [ path [ d "M32.562 40.952L0 20.475 32.562 0l32.564 20.475z", fill "#d8dded" ]
            []
        ]
