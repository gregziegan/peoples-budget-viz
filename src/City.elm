module City exposing (Budget, City, changeBudget, init, initialBudget, visualization)

import City.Building as Building exposing (BuildingType(..))
import City.Education as Education
import City.Hospital as Hospital exposing (HospitalType(..))
import City.Housing as Housing exposing (HousingType(..))
import City.Park as Park exposing (ParkType(..))
import City.Road as Road exposing (RoadType(..))
import City.Tile as Tile exposing (Tile)
import City.TileType exposing (TileType(..))
import Element exposing (Element, px, shrink)
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)


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

        Road roadType ->
            Road.view tile roadType

        BlankTile ->
            Tile.blank tile


visualization : City -> Element msg
visualization city =
    let
        tile =
            Tile.default

        parks =
            List.map (viewTile tile << Park << parkFromId) (List.range 1 city.parks)

        t =
            viewTile tile

        street maybePark =
            [ svg [ Attr.x "800", Attr.y "274" ] [ t (Road Corner) ]
            , svg [ Attr.x "700", Attr.y "337" ] [ t (Road Straight) ]
            , svg [ Attr.x "600", Attr.y "400" ] [ viewTile { tile | transform = "rotate(180 100 100)" } (Road Corner) ]
            , svg [ Attr.x "550", Attr.y "295" ] [ t (Building Skyscraper) ]
            , svg [ Attr.x "860", Attr.y "260" ] [ t (Housing MediumApartment) ]
            , svg [ Attr.x "700", Attr.y "463" ] [ t (Road Crosswalk) ]
            , svg [ Attr.x "800", Attr.y "526" ] [ t (Road FourWay) ]
            , svg [ Attr.x "900", Attr.y "463" ] [ t (Road Straight) ]
            ]
                ++ (Maybe.withDefault [ svg [ Attr.x "760", Attr.y "405" ] [ t BlankTile ] ] <| Maybe.map (List.singleton << svg [ Attr.x "760", Attr.y "405" ] << List.singleton) maybePark)
    in
    Element.column [ Element.centerX ]
        [ Element.el [ Element.width shrink, Element.height shrink ]
            (Element.html
                (svg
                    [ style "border: 1px grey solid", Attr.height "1200", Attr.width "1600" ]
                    (street (List.head parks))
                )
            )
        ]
