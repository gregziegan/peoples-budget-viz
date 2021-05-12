module City.Tile exposing (Rotation(..), Tile, blank, def, default, id, position, rotate)

import City.Building as Building exposing (BuildingType(..))
import City.Education as Education
import City.Hospital as Hospital exposing (HospitalType(..))
import City.Housing as Housing exposing (HousingType(..))
import City.Park as Park exposing (ParkType(..))
import City.TileType exposing (TileType(..))
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


originX =
    0


originY =
    7


offset : Tile -> ( Int, Int )
offset tile =
    ( tile.x + originX + tile.y, originY - tile.y + tile.x )


position : Tile -> List (Svg.Attribute msg)
position tile =
    let
        ( x, y ) =
            offset tile
    in
    [ Attr.width (String.fromInt tile.width)
    , Attr.height (String.fromInt tile.height)
    , Attr.x (String.fromFloat <| (toFloat x * 95))
    , Attr.y (String.fromFloat <| (toFloat y * 60))
    ]


rotate : Tile -> List (Svg.Attribute msg)
rotate tile =
    let
        ( x, y ) =
            offset tile

        rotInDeg =
            case tile.rotation of
                RNone ->
                    0

                RQuarter ->
                    0

                RHalf ->
                    -180

                RThreeQuarters ->
                    -180

        midX =
            -- (x * 95) +
            tile.width // 2

        midY =
            -- (y * 60) +
            tile.height // 2
    in
    [ Attr.transform ("rotate(" ++ String.fromInt rotInDeg ++ " " ++ String.fromInt midX ++ " " ++ String.fromInt midY ++ ")") ]


def : TileType -> Svg msg
def tileType =
    case tileType of
        Building buildingType ->
            Building.def buildingType

        Hospital hospitalType ->
            Hospital.def hospitalType

        Park parkType ->
            Park.def parkType

        Education ->
            Education.def

        Housing housingType ->
            Housing.def housingType

        BlankTile ->
            blank


id : TileType -> String
id tileType =
    case tileType of
        Building buildingType ->
            Building.id buildingType

        Hospital hospitalType ->
            Hospital.id hospitalType

        Park parkType ->
            Park.id parkType

        Education ->
            Education.id

        Housing housingType ->
            Housing.id housingType

        BlankTile ->
            "blank"


blank : Svg msg
blank =
    svg [ Attr.id (id BlankTile), viewBox "0 0 65.126 40.952" ]
        [ path [ d "M32.562 40.952L0 20.475 32.562 0l32.564 20.475z", fill "#d8dded" ]
            []
        ]
