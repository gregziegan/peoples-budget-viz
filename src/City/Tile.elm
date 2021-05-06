module City.Tile exposing (Rotation(..), Tile, attrs, blank, default)

import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox, x)


type alias Tile =
    { width : Int
    , height : Int
    , rotation : Rotation
    , x : Int
    , y : Int
    }


type Rotation
    = RNone
    | RQuarter
    | RHalf
    | RThreeQuarters


default : ( Int, Int ) -> Rotation -> Tile
default ( x, y ) rotation =
    { height = 200
    , width = 200
    , rotation = rotation
    , x = x
    , y = y
    }


attrs : Tile -> List (Svg.Attribute msg)
attrs tile =
    let
        rotInDeg =
            case tile.rotation of
                RNone ->
                    0

                RQuarter ->
                    0

                RHalf ->
                    -180

                RThreeQuarters ->
                    0

        ( originX, originY ) =
            ( 7, 1 )

        ( x, y ) =
            ( tile.x + originX - tile.y, tile.y + originY + tile.x )

        midX =
            (x * 95) + (tile.width // 2)

        midY =
            (y * 60) + (tile.height // 2)
    in
    [ Attr.width (String.fromInt tile.width)
    , Attr.height (String.fromInt tile.height)
    , Attr.x (String.fromFloat <| (toFloat x * 95))
    , Attr.y (String.fromFloat <| (toFloat y * 60))
    , transform ("rotate(" ++ String.fromInt rotInDeg ++ " " ++ String.fromInt midX ++ " " ++ String.fromInt midY ++ ")")
    ]


blank : Tile -> Svg msg
blank tile =
    svg (attrs tile ++ [ viewBox "0 0 65.126 40.952" ])
        [ path [ d "M32.562 40.952L0 20.475 32.562 0l32.564 20.475z", fill "#d8dded" ]
            []
        ]
