module City exposing (Budget, City, CityTile, changeBudget, generate, init, initialBudget, visualization)

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


type CityTile
    = Connecting Road
    | EmptyPlot


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


generate : Seed -> ( Board CityTile, Seed )
generate seed =
    Tiler.generateBoard cityWidth cityHeight generateOneOf validateNeighbors seed


generateOneOf : ( Int, Int ) -> ( CityTile, List CityTile )
generateOneOf _ =
    ( Connecting (Road Straight RNone)
    , [ Connecting (Road Straight RQuarter)
      , Connecting (Road Corner RNone)
      , Connecting (Road Corner RQuarter)
      , Connecting (Road Corner RHalf)
      , Connecting (Road Corner RThreeQuarters)
      , Connecting (Road Junction RNone)
      , Connecting (Road Tee RNone)
      , Connecting (Road Tee RQuarter)
      , Connecting (Road Tee RHalf)
      , Connecting (Road Tee RThreeQuarters)
      , EmptyPlot
      , EmptyPlot
      , EmptyPlot
      ]
    )


validateNeighbors : List CityTile -> List CityTile -> Neighbor -> ( CityTile, List CityTile )
validateNeighbors possibleThisTiles possibleNeighbors neighborDirection =
    let
        filteredPossibilities =
            List.filter (\neigh -> List.any (\self -> validTile neighborDirection self neigh) possibleThisTiles) possibleNeighbors
    in
    case filteredPossibilities of
        t :: others ->
            ( t, others )

        [] ->
            -- TODO: Find a way to never reach this
            ( EmptyPlot, [] )


flipDirection direction =
    case direction of
        North ->
            South

        West ->
            East

        South ->
            North

        East ->
            West


validPlot : Neighbor -> Road -> Bool
validPlot neighborDirection road =
    case ( road.style, road.rotation, neighborDirection ) of
        ( Straight, RNone, North ) ->
            True

        ( Straight, RHalf, North ) ->
            True

        ( Straight, RQuarter, North ) ->
            False

        ( Straight, RThreeQuarters, North ) ->
            False

        ( Straight, RNone, South ) ->
            True

        ( Straight, RHalf, South ) ->
            True

        ( Straight, RQuarter, South ) ->
            False

        ( Straight, RThreeQuarters, South ) ->
            False

        ( Straight, RNone, West ) ->
            False

        ( Straight, RHalf, West ) ->
            False

        ( Straight, RQuarter, West ) ->
            True

        ( Straight, RThreeQuarters, West ) ->
            True

        ( Straight, RNone, East ) ->
            False

        ( Straight, RHalf, East ) ->
            False

        ( Straight, RQuarter, East ) ->
            True

        ( Straight, RThreeQuarters, East ) ->
            True

        ( Corner, RNone, North ) ->
            False

        ( Corner, RHalf, North ) ->
            True

        ( Corner, RQuarter, North ) ->
            True

        ( Corner, RThreeQuarters, North ) ->
            False

        ( Corner, RNone, South ) ->
            True

        ( Corner, RHalf, South ) ->
            False

        ( Corner, RQuarter, South ) ->
            False

        ( Corner, RThreeQuarters, South ) ->
            True

        ( Corner, RNone, West ) ->
            False

        ( Corner, RHalf, West ) ->
            True

        ( Corner, RQuarter, West ) ->
            False

        ( Corner, RThreeQuarters, West ) ->
            True

        ( Corner, RNone, East ) ->
            True

        ( Corner, RHalf, East ) ->
            False

        ( Corner, RQuarter, East ) ->
            True

        ( Corner, RThreeQuarters, East ) ->
            False

        ( Junction, RNone, North ) ->
            False

        ( Junction, RHalf, North ) ->
            False

        ( Junction, RQuarter, North ) ->
            False

        ( Junction, RThreeQuarters, North ) ->
            False

        ( Junction, RNone, South ) ->
            False

        ( Junction, RHalf, South ) ->
            False

        ( Junction, RQuarter, South ) ->
            False

        ( Junction, RThreeQuarters, South ) ->
            False

        ( Junction, RNone, West ) ->
            False

        ( Junction, RHalf, West ) ->
            False

        ( Junction, RQuarter, West ) ->
            False

        ( Junction, RThreeQuarters, West ) ->
            False

        ( Junction, RNone, East ) ->
            False

        ( Junction, RHalf, East ) ->
            False

        ( Junction, RQuarter, East ) ->
            False

        ( Junction, RThreeQuarters, East ) ->
            False

        ( Tee, RNone, North ) ->
            False

        ( Tee, RHalf, North ) ->
            False

        ( Tee, RQuarter, North ) ->
            True

        ( Tee, RThreeQuarters, North ) ->
            False

        ( Tee, RNone, South ) ->
            False

        ( Tee, RHalf, South ) ->
            False

        ( Tee, RQuarter, South ) ->
            False

        ( Tee, RThreeQuarters, South ) ->
            True

        ( Tee, RNone, West ) ->
            False

        ( Tee, RHalf, West ) ->
            True

        ( Tee, RQuarter, West ) ->
            True

        ( Tee, RThreeQuarters, West ) ->
            False

        ( Tee, RNone, East ) ->
            True

        ( Tee, RHalf, East ) ->
            False

        ( Tee, RQuarter, East ) ->
            False

        ( Tee, RThreeQuarters, East ) ->
            False


validTile : Neighbor -> CityTile -> CityTile -> Bool
validTile neighborDirection self neighbor =
    case ( self, neighborDirection ) of
        ( Connecting road, _ ) ->
            case neighbor of
                Connecting neighborRoad ->
                    validJunction neighborDirection road neighborRoad

                EmptyPlot ->
                    validPlot neighborDirection road

        ( EmptyPlot, _ ) ->
            case neighbor of
                Connecting neighborRoad ->
                    validPlot (flipDirection neighborDirection) neighborRoad

                EmptyPlot ->
                    True


validJunction : Neighbor -> Road -> Road -> Bool
validJunction neighborDirection self neighbor =
    case ( self.style, self.rotation, neighborDirection ) of
        -- Straight
        -- Straight, North neighbor
        ( Straight, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        ( Straight, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        ( Straight, RQuarter, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Straight, South neighbor
        ( Straight, RNone, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Straight, RThreeQuarters, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Straight, East neighbor
        ( Straight, RNone, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Straight, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Straight, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Straight, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Straight, West neighbor
        ( Straight, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Straight, RHalf, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Straight, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        ( Straight, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        -- Corner
        -- Corner, North neighbor
        ( Corner, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RQuarter, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        ( Corner, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RNone ) ->
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
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Corner, RThreeQuarters, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Corner, East neighbor
        ( Corner, RNone, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Corner, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Corner, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Corner, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Corner, West neighbor
        ( Corner, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Corner, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        ( Corner, RHalf, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                _ ->
                    False

        ( Corner, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        -- Junction
        -- Junction, North neighbor
        ( Junction, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Junction, RQuarter, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Junction, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Junction, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        -- Junction, South neighbor
        ( Junction, RNone, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Junction, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Junction, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Junction, RThreeQuarters, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Junction, East neighbor
        ( Junction, RNone, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        -- Junction, West neighbor
        ( Junction, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RHalf, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Junction, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        -- Tee
        -- Tee, North neighbor
        ( Tee, RNone, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RThreeQuarters, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                _ ->
                    False

        -- Tee, South neighbor
        ( Tee, RNone, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RHalf, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        ( Tee, RThreeQuarters, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Junction, _ ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Tee, East neighbor
        ( Tee, RNone, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Tee, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Tee, RHalf, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Tee, RThreeQuarters, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        -- Tee, West neighbor
        ( Tee, RNone, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Tee, RQuarter, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Tee, RHalf, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RNone ) ->
                    True

                ( Straight, RHalf ) ->
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

        ( Tee, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight, RQuarter ) ->
                    True

                ( Straight, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
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


drawRoad : Tile -> Road -> Svg msg
drawRoad tile { style } =
    Road.view tile style


drawCityTile : ( Int, Int ) -> CityTile -> Svg msg
drawCityTile ( x, y ) cityTile =
    case cityTile of
        EmptyPlot ->
            Tile.blank (Tile.default ( x, y ) RNone)

        Connecting ({ rotation } as road) ->
            let
                tile =
                    Tile.default ( x, y ) rotation
            in
            drawRoad tile road


drawUndecided : ( Int, Int ) -> String -> Svg msg
drawUndecided position style =
    Tile.blank (Tile.default position RNone)


cityTileToString : CityTile -> String
cityTileToString cityTile =
    case cityTile of
        EmptyPlot ->
            "Empty Plot"

        Connecting { style } ->
            case style of
                Straight ->
                    "Straight"

                Junction ->
                    "Junction"

                Tee ->
                    "Tee"

                Corner ->
                    "Corner"



--Crosswalk ->
--"Crosswalk"


drawTile : ( Int, Int ) -> ( CityTile, List CityTile ) -> Svg msg
drawTile pos ( cityTile, cityTiles ) =
    case cityTiles of
        [] ->
            drawCityTile pos cityTile

        _ ->
            drawUndecided pos (String.join "\n" (List.map cityTileToString (cityTile :: cityTiles)))


visualization : Board CityTile -> City -> Element msg
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
