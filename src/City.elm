module City exposing (Budget, City, changeBudget, init, initialBudget, visualization)

import City.Building as Building exposing (BuildingType(..))
import City.Education as Education
import City.Hospital as Hospital exposing (HospitalType(..))
import City.Housing as Housing exposing (HousingType(..))
import City.Park as Park exposing (ParkType(..))
import City.Road as Road exposing (Road, RoadType(..), Rotation(..))
import City.Tile as Tile exposing (Tile)
import City.TileType exposing (TileType(..))
import Element exposing (Element, px, shrink)
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)
import Tiler exposing (Board, Neighbor(..))


type alias Budget =
    { parks : Float }


type alias City =
    { parks : Int
    , housing : Int
    , parkingLots : Int
    }


init :
    { parks : Int
    , housing : Int
    , parkingLots : Int
    }
    -> City
init { parks, housing, parkingLots } =
    { parks = parks, housing = housing, parkingLots = parkingLots }


initialBudget : Budget
initialBudget =
    { parks = 1 }


changeBudget : Budget -> City -> City
changeBudget budget city =
    { city | parks = round budget.parks }


parkFromId : Int -> ParkType
parkFromId id =
    if modBy 2 id == 0 then
        Forest

    else
        Lawn


viewTile : Tile -> TileType -> Svg msg
viewTile tile tileType =
    case tileType of
        Building buildingType ->
            Building.view tile buildingType

        Hospital hospitalType ->
            Hospital.view tile hospitalType

        Park parkType ->
            Park.view tile parkType

        Education ->
            Education.highSchool tile

        Housing housingType ->
            Housing.view tile housingType

        BlankTile ->
            Tile.blank tile


drawRoad : ( Int, Int ) -> Road -> Svg msg
drawRoad ( x, y ) { style, rotation } =
    let
        tile =
            Tile.default ( x, y )

        rotInDeg =
            case rotation of
                RNone ->
                    "0"

                RQuarter ->
                    "-90"

                RHalf ->
                    "-180"

                RThreeQuarters ->
                    "-270"
    in
    Road.view { tile | transform = "rotate(" ++ rotInDeg ++ " 100 100)" } style


drawUndecided : ( Int, Int ) -> String -> Svg msg
drawUndecided position style =
    Tile.blank (Tile.default position)


cellStyleToString : RoadType -> String
cellStyleToString style =
    case style of
        Straight ->
            "Straight"

        Junction ->
            "Junction"

        Tee ->
            "Tee"

        Corner ->
            "Corner"

        DeadEnd ->
            "Dead End"


drawTile : ( Int, Int ) -> ( Road, List Road ) -> Svg msg
drawTile pos ( road, roads ) =
    case roads of
        [] ->
            drawRoad pos road

        _ ->
            drawUndecided pos (String.join "\n" (List.map (\{ style } -> cellStyleToString style) (road :: roads)))


visualization : Board Road -> City -> Element msg
visualization board city =
    Element.column [ Element.centerX ]
        [ Element.el [ Element.width shrink, Element.height shrink ]
            (Element.html
                (svg
                    [ style "border: 1px grey solid", Attr.height "1200", Attr.width "1600" ]
                    (Tiler.map drawTile board)
                )
            )
        ]
