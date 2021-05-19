module City.Tile exposing (Rotation(..), Tile, blank, def, id, position, rotate)

import City.Building as Building exposing (BuildingType(..))
import City.Education as Education
import City.Hospital as Hospital exposing (HospitalType(..))
import City.Housing as Housing exposing (HousingType(..))
import City.Park as Park exposing (ParkType(..))
import City.Size exposing (CitySize(..))
import City.TileType exposing (TileType(..))
import Metadata exposing (Window)
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox, x)


type alias Tile =
    { dimens : { width : Float, height : Float }
    , size : CitySize
    , rotation : Rotation
    , x : Int
    , y : Int
    }


type Rotation
    = RNone
    | RQuarter
    | RHalf
    | RThreeQuarters


scaleTile : Tile -> Float
scaleTile tile =
    case tile.size of
        Small ->
            tile.dimens.width / 10

        Medium ->
            tile.dimens.width / 12

        Large ->
            tile.dimens.width / 19


offset : Tile -> ( Float, Float )
offset tile =
    let
        scale =
            scaleTile tile

        ( originX, originY ) =
            case tile.size of
                Small ->
                    ( 2, tile.dimens.height / scale / 0.5 )

                Medium ->
                    ( (tile.dimens.width / scale) / 6, (tile.dimens.height / scale) / 0.6 )

                Large ->
                    ( (tile.dimens.width / scale) / 10, (tile.dimens.height / scale) / 0.67 )
    in
    ( toFloat tile.x + originX + toFloat tile.y, originY - toFloat tile.y + toFloat tile.x )


position : Tile -> List (Svg.Attribute msg)
position tile =
    let
        ( x, y ) =
            offset tile

        scale =
            scaleTile tile
    in
    [ Attr.width (String.fromFloat scale)
    , Attr.height (String.fromFloat scale)

    -- , Attr.x (String.fromFloat <| (toFloat x * 95 / 2))
    -- , Attr.y (String.fromFloat <| (toFloat y * 60 / 2))
    , Attr.x (String.fromFloat <| (x * scale * 0.95 / 2))
    , Attr.y (String.fromFloat <| (y * scale * 0.6 / 2))
    ]


rotate : Tile -> List (Svg.Attribute msg)
rotate tile =
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
                    -180

        mid =
            scaleTile tile / 2
    in
    [ Attr.transform ("rotate(" ++ String.fromInt rotInDeg ++ " " ++ String.fromFloat mid ++ " " ++ String.fromFloat mid ++ ")") ]


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
