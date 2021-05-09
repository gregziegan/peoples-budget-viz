module City exposing (Budget, City, changeBudget, generate, init, initialBudget, visualization)

import City.Building as Building exposing (BuildingType(..))
import City.Education as Education
import City.Hospital as Hospital exposing (HospitalType(..))
import City.Housing as Housing exposing (HousingType(..))
import City.Park as Park exposing (ParkType(..))
import City.Road as Road exposing (Road, RoadType(..))
import City.Tile as Tile exposing (Rotation(..), Tile)
import City.TileType exposing (TileType(..))
import Element exposing (Element, px, shrink)
import Random exposing (Seed)
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


cityWidth =
    8


cityHeight =
    8


generate : Seed -> ( Board Road, Seed )
generate seed =
    Tiler.generateBoard cityWidth cityHeight generateOneOf validateNeighbors seed


generateOneOf : ( Int, Int ) -> ( Road, List Road )
generateOneOf _ =
    ( Road (Straight { hasCrosswalk = False }) RNone
    , [ Road (Straight { hasCrosswalk = False }) RQuarter
      , Road (Straight { hasCrosswalk = True }) RNone
      , Road (Straight { hasCrosswalk = True }) RQuarter
      , Road Corner RNone
      , Road Corner RQuarter
      , Road Corner RHalf
      , Road Corner RThreeQuarters
      , Road Junction RNone
      , Road Tee RNone
      , Road Tee RQuarter
      , Road Tee RHalf
      , Road Tee RThreeQuarters
      , Road Empty RNone
      ]
    )


validateNeighbors : List Road -> List Road -> Neighbor -> ( Road, List Road )
validateNeighbors possibleThisTiles possibleNeighbors neighborDirection =
    let
        filteredPossibilities =
            List.filter (\neigh -> List.any (\self -> validJunction neighborDirection self neigh) possibleThisTiles) possibleNeighbors
    in
    case filteredPossibilities of
        t :: others ->
            ( t, others )

        [] ->
            -- TODO: Find a way to never reach this
            ( Road Junction RNone, [] )


validJunction : Neighbor -> Road -> Road -> Bool
validJunction neighborDirection self neighbor =
    case ( self.style, self.rotation, neighborDirection ) of
        -- Straight
        -- Straight _, North neighbor
        ( Straight _, RQuarter, North ) ->
            neighbor.style == Empty

        ( Straight _, RThreeQuarters, North ) ->
            neighbor.style == Empty

        ( Straight _, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight _, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Straight _, South neighbor
        ( Straight _, RQuarter, South ) ->
            neighbor.style == Empty

        ( Straight _, RThreeQuarters, South ) ->
            neighbor.style == Empty

        ( Straight _, RNone, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Straight _, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Straight _, East neighbor
        ( Straight _, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight _, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight _, RNone, East ) ->
            neighbor.style == Empty

        ( Straight _, RHalf, East ) ->
            neighbor.style == Empty

        -- Straight _, West neighbor
        ( Straight _, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight _, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight _, RNone, West ) ->
            neighbor.style == Empty

        ( Straight _, RHalf, West ) ->
            neighbor.style == Empty

        -- Corner
        -- Corner, North neighbor
        ( Corner, RNone, North ) ->
            neighbor.style == Empty

        ( Corner, RQuarter, North ) ->
            neighbor.style == Empty

        ( Corner, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        -- Corner, South neighbor
        ( Corner, RNone, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        ( Corner, RHalf, South ) ->
            neighbor.style == Empty

        ( Corner, RThreeQuarters, South ) ->
            neighbor.style == Empty

        -- Corner, East neighbor
        ( Corner, RNone, East ) ->
            neighbor.style == Empty

        ( Corner, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        ( Corner, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RThreeQuarters, East ) ->
            neighbor.style == Empty

        -- Corner, West neighbor
        ( Corner, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        ( Corner, RQuarter, West ) ->
            neighbor.style == Empty

        ( Corner, RHalf, West ) ->
            neighbor.style == Empty

        ( Corner, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Junction
        -- Junction, North neighbor
        ( Junction, _, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        -- Junction, South neighbor
        ( Junction, _, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        -- Junction, East neighbor
        ( Junction, _, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Junction, West neighbor
        ( Junction, _, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Tee
        -- Tee, North neighbor
        ( Tee, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RHalf, North ) ->
            neighbor.style == Empty

        ( Tee, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        -- Tee, South neighbor
        ( Tee, RNone, South ) ->
            neighbor.style == Empty

        ( Tee, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RThreeQuarters, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

        -- Tee, East neighbor
        ( Tee, RNone, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, East ) ->
            neighbor.style == Empty

        ( Tee, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Tee, West neighbor
        ( Tee, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RHalf, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RThreeQuarters, West ) ->
            neighbor.style == Empty

        ( Empty, _, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Empty, _ ) ->
                    True

                _ ->
                    False

        ( Empty, _, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Empty, _ ) ->
                    True

                _ ->
                    False

        ( Empty, _, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                ( Empty, _ ) ->
                    True

                _ ->
                    False

        ( Empty, _, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Empty, _ ) ->
                    True

                _ ->
                    False


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
            Tile.default ( x, y ) rotation
    in
    Road.view tile style


drawUndecided : ( Int, Int ) -> String -> Svg msg
drawUndecided position style =
    Tile.blank (Tile.default position RNone)


cellStyleToString : RoadType -> String
cellStyleToString style =
    case style of
        Straight { hasCrosswalk } ->
            if hasCrosswalk then
                "Crosswalk"

            else
                "Straight"

        Junction ->
            "Junction"

        Tee ->
            "Tee"

        Corner ->
            "Corner"

        Empty ->
            "Empty"



--Crosswalk ->
--"Crosswalk"


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
