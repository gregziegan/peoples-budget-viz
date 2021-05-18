module City exposing (Budget, City, Range, TotalBudget, currentBudget, generate, total, totalBudget, visualization)

import City.Building exposing (BuildingType(..))
import City.Hospital exposing (HospitalType(..))
import City.Housing exposing (HousingType(..))
import City.Park exposing (ParkType(..))
import City.Road as Road exposing (Road, RoadType(..))
import City.Tile as Tile exposing (Rotation(..), Tile)
import City.TileType exposing (TileType(..))
import Element exposing (Element, px, shrink)
import Random exposing (Seed)
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)
import Tiler exposing (Board, Neighbor(..))


type alias City =
    Board Road


type alias TotalBudget =
    Float


totalBudget : TotalBudget
totalBudget =
    1056


total : Budget -> Int
total { police, housing, transit, health, parks } =
    round <| police.current + housing.current + transit.current + health.current + parks.current


type alias Range =
    { current : Float
    , min : Float
    , max : Float
    }


type alias Budget =
    { police : Range
    , housing : Range
    , transit : Range
    , health : Range
    , parks : Range
    }


currentBudget : Budget
currentBudget =
    { police = Range (0.28 * totalBudget) 0 (0.28 * totalBudget)
    , housing = Range (0.0097 * totalBudget) (0.0097 * totalBudget) (0.26 * totalBudget)
    , transit = Range (0.0591 * totalBudget) (0.0591 * totalBudget) (0.26 * totalBudget)
    , health = Range (0.0932 * totalBudget) (0.0932 * totalBudget) (0.26 * totalBudget)
    , parks = Range (0.0707 * totalBudget) (0.0707 * totalBudget) (0.26 * totalBudget)
    }


cityWidth =
    16


cityHeight =
    16


generate : Budget -> Seed -> ( Board Road, Seed )
generate budget seed =
    Tiler.generateBoard cityWidth cityHeight (generateOneOf budget) validateNeighbors seed


rangeMultiplier : Range -> List Road -> List Road
rangeMultiplier range roads =
    let
        step =
            range.max / 5

        multiplier =
            ceiling <| range.current / step
    in
    List.map (List.repeat multiplier) roads
        |> List.concat


generateOneOf : Budget -> ( Int, Int ) -> ( Road, List Road )
generateOneOf budget ( x, y ) =
    let
        police =
            rangeMultiplier budget.police [ Road (Empty (Building PoliceStation)) RNone ]

        lowDensityHousing =
            rangeMultiplier budget.housing
                [ Road (Empty (Housing House)) RNone
                , Road (Empty (Housing LargeHouse)) RNone
                ]

        highDensityHousing =
            rangeMultiplier budget.housing
                [ Road (Empty (Housing <| TallApartment 1)) RNone
                , Road (Empty (Housing <| TallApartment 2)) RNone
                , Road (Empty (Housing <| TallApartment 3)) RNone
                , Road (Empty (Housing <| MediumApartment)) RNone
                ]

        transit =
            rangeMultiplier budget.transit [ Road (Empty (Building TrainStation)) RNone ]

        health =
            rangeMultiplier budget.health
                [ Road (Empty (Hospital Clinic)) RNone
                , Road (Empty (Hospital Large)) RNone
                ]

        parks =
            rangeMultiplier budget.parks
                [ Road (Empty (Park Playground)) RNone
                , Road (Empty (Park Lawn)) RNone
                , Road (Empty (Park Forest)) RNone
                ]

        roads =
            [ Road (Straight { hasCrosswalk = False }) RQuarter
            , Road (Straight { hasCrosswalk = True }) RNone
            , Road (Straight { hasCrosswalk = True }) RQuarter

            --   , Road Corner RNone
            --   , Road Corner RQuarter
            --   , Road Corner RHalf
            --   , Road Corner RThreeQuarters
            , Road Junction RNone
            , Road Tee RNone
            , Road Tee RQuarter
            , Road Tee RHalf
            , Road Tee RThreeQuarters
            , Road (Empty BlankTile) RNone
            ]

        tallBuildings =
            [ Road (Empty (Building Skyscraper)) RNone
            , Road (Empty (Building DepartmentStore)) RNone
            , Road (Empty (Building MediumMultiuse)) RNone
            , Road (Empty (Building Office)) RNone
            , Road (Empty (Building LargeOffice)) RNone
            ]
                ++ highDensityHousing
                ++ health
                ++ parks
                ++ police

        buildings =
            [ Road (Empty (Hospital Clinic)) RNone
            , Road (Empty (Park Playground)) RNone
            , Road (Empty (Building FastFood)) RNone
            , Road (Empty (Building Grocery)) RNone
            , Road (Empty (Building Shop)) RNone
            , Road (Empty (Building DepartmentStore)) RNone
            , Road (Empty (Building SmallMultiuse)) RNone
            , Road (Empty (Building Office)) RNone
            ]
                ++ lowDensityHousing
                ++ health
                ++ parks
                ++ police
                ++ transit

        inCityCenter =
            ((toFloat x / cityWidth)
                < 0.7
                && (toFloat x / cityWidth)
                > 0.3
            )
                && ((toFloat y / cityHeight)
                        < 0.7
                        && (toFloat y / cityHeight)
                        > 0.3
                   )
    in
    ( Road (Straight { hasCrosswalk = False }) RNone
    , if x == 0 && y == 0 then
        [ Road Corner RHalf ]

      else if x == cityWidth - 1 && y == 0 then
        [ Road Corner RThreeQuarters ]

      else if x == 0 && y == cityHeight - 1 then
        [ Road Corner RQuarter ]

      else if x == cityWidth - 1 && y == cityHeight - 1 then
        [ Road Corner RNone ]

      else if (x == 1 || x == cityWidth - 2) && (y == 0 || y == cityHeight - 1) then
        [ Road (Straight { hasCrosswalk = False }) RQuarter ]

      else if (y == 1 || y == cityHeight - 2) && (x == 0 || x == cityWidth - 1) then
        [ Road (Straight { hasCrosswalk = False }) RNone ]

      else if x == 0 then
        [ Road Tee RHalf, Road (Straight { hasCrosswalk = False }) RNone ]

      else if y == 0 then
        [ Road Tee RThreeQuarters, Road (Straight { hasCrosswalk = False }) RQuarter ]

      else if x == cityWidth - 1 then
        [ Road Tee RNone, Road (Straight { hasCrosswalk = False }) RNone ]

      else if y == cityHeight - 1 then
        [ Road Tee RQuarter, Road (Straight { hasCrosswalk = False }) RQuarter ]

      else if x == 1 || x == cityWidth - 2 then
        Road (Straight { hasCrosswalk = False }) RQuarter :: buildings

      else if y == 1 || y == cityHeight - 2 then
        Road (Straight { hasCrosswalk = False }) RNone :: buildings

      else if inCityCenter then
        roads ++ tallBuildings

      else
        roads ++ buildings
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
            ( Road (Straight { hasCrosswalk = True }) RNone, [] )


isEmptyRoad : Road -> Bool
isEmptyRoad neighbor =
    case neighbor.style of
        Empty _ ->
            True

        _ ->
            False


validJunction : Neighbor -> Road -> Road -> Bool
validJunction neighborDirection self neighbor =
    case ( self.style, self.rotation, neighborDirection ) of
        -- Straight
        -- Straight _, North neighbor
        ( Straight _, RQuarter, North ) ->
            isEmptyRoad neighbor

        ( Straight _, RThreeQuarters, North ) ->
            isEmptyRoad neighbor

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

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
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

                ( Tee, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                _ ->
                    False

        -- Straight _, South neighbor
        ( Straight _, RQuarter, South ) ->
            isEmptyRoad neighbor

        ( Straight _, RThreeQuarters, South ) ->
            isEmptyRoad neighbor

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

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
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

                ( Tee, RHalf ) ->
                    True

                ( Tee, RThreeQuarters ) ->
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
            isEmptyRoad neighbor

        ( Straight _, RHalf, East ) ->
            isEmptyRoad neighbor

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
            isEmptyRoad neighbor

        ( Straight _, RHalf, West ) ->
            isEmptyRoad neighbor

        -- Corner
        -- Corner, North neighbor
        ( Corner, RNone, North ) ->
            isEmptyRoad neighbor

        ( Corner, RQuarter, North ) ->
            isEmptyRoad neighbor

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
            isEmptyRoad neighbor

        ( Corner, RThreeQuarters, South ) ->
            isEmptyRoad neighbor

        -- Corner, East neighbor
        ( Corner, RNone, East ) ->
            isEmptyRoad neighbor

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
            isEmptyRoad neighbor

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
            isEmptyRoad neighbor

        ( Corner, RHalf, West ) ->
            isEmptyRoad neighbor

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
            isEmptyRoad neighbor

        ( Tee, RHalf, North ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

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
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                _ ->
                    False

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
            isEmptyRoad neighbor

        -- Tee, East neighbor
        ( Tee, RNone, East ) ->
            isEmptyRoad neighbor

        ( Tee, RQuarter, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

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
            isEmptyRoad neighbor

        ( Tee, RThreeQuarters, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                _ ->
                    False

        ( Empty _, _, North ) ->
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

                ( Empty _, _ ) ->
                    True

                _ ->
                    False

        ( Empty _, _, South ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RQuarter ) ->
                    True

                ( Straight _, RThreeQuarters ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RQuarter ) ->
                    True

                ( Empty _, _ ) ->
                    True

                _ ->
                    False

        ( Empty _, _, West ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RHalf ) ->
                    True

                ( Corner, RThreeQuarters ) ->
                    True

                ( Tee, RNone ) ->
                    True

                ( Empty _, _ ) ->
                    True

                _ ->
                    False

        ( Empty _, _, East ) ->
            case ( neighbor.style, neighbor.rotation ) of
                ( Straight _, RNone ) ->
                    True

                ( Straight _, RHalf ) ->
                    True

                ( Corner, RNone ) ->
                    True

                ( Corner, RQuarter ) ->
                    True

                ( Tee, RHalf ) ->
                    True

                ( Empty _, _ ) ->
                    True

                _ ->
                    False


drawRoad : ( Int, Int ) -> Road -> ( Svg msg, { x : Int, y : Int, style : RoadType } )
drawRoad ( x, y ) road =
    let
        tile =
            Tile.default ( x, y ) road.rotation
    in
    ( Road.view tile road
    , { x = x
      , y = y
      , style = road.style
      }
    )


drawUndecided : ( Int, Int ) -> String -> ( Svg msg, TileInfo )
drawUndecided ( x, y ) style =
    ( Tile.blank, { x = x, y = y, style = Empty BlankTile } )


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

        Empty _ ->
            "Empty"


type alias TileInfo =
    { x : Int, y : Int, style : RoadType }


drawTile : ( Int, Int ) -> ( Road, List Road ) -> ( Svg msg, TileInfo )
drawTile pos ( road, roads ) =
    case roads of
        [] ->
            drawRoad pos road

        _ ->
            drawUndecided pos (String.join "\n" (List.map (\{ style } -> cellStyleToString style) (road :: roads)))


allTiles =
    List.concat
        [ [ Road (Straight { hasCrosswalk = False }) RNone
          , Road (Straight { hasCrosswalk = False }) RQuarter
          , Road (Straight { hasCrosswalk = True }) RNone
          , Road (Straight { hasCrosswalk = True }) RQuarter
          , Road Junction RNone
          , Road Corner RNone
          , Road Corner RQuarter
          , Road Tee RNone
          , Road Tee RQuarter
          ]
        , List.map (\tile -> Road (Empty tile) RNone)
            [ Housing House
            , Housing LargeHouse
            , Housing MediumApartment
            , Housing (TallApartment 0)
            , Housing (TallApartment 1)
            , Housing (TallApartment 2)
            , Building Skyscraper
            , Building Shop
            , Building DepartmentStore
            , Building SmallMultiuse
            , Building MediumMultiuse
            , Building Grocery
            , Building FastFood
            , Building Office
            , Building LargeOffice
            , Education
            , Hospital Clinic
            , Hospital Large
            , Park TennisCourt
            , Park Forest
            , Park Lawn
            , Park Playground
            , Building PoliceStation
            , Building TrainStation
            , BlankTile
            ]
        ]


defs : List (Svg msg)
defs =
    [ Svg.defs [] (List.map Road.def allTiles) ]


visualization : City -> Element msg
visualization city =
    let
        tiles =
            Tiler.map drawTile city

        buildings =
            tiles
                |> List.filterMap
                    (\( tile, info ) ->
                        case info.style of
                            Empty BlankTile ->
                                Nothing

                            Empty _ ->
                                Just ( tile, info )

                            _ ->
                                Nothing
                    )
                |> List.sortWith
                    (\( tile, info ) ( tile2, info2 ) ->
                        if info.x < info2.x || info.y > info2.y then
                            LT

                        else if info.x == info2.x && info.y == info2.y then
                            EQ

                        else
                            GT
                    )
                |> List.map Tuple.first

        roads =
            List.map
                (\( tile, info ) ->
                    case info.style of
                        Empty BlankTile ->
                            tile

                        Empty _ ->
                            drawRoad ( info.x, info.y ) (Road (Empty BlankTile) RNone)
                                |> Tuple.first

                        _ ->
                            tile
                )
                tiles
    in
    Element.column [ Element.centerX ]
        [ Element.el [ Element.width shrink, Element.height shrink ]
            (Element.html
                (svg
                    [ style "border: 1px grey solid", Attr.height "1200", Attr.width "1600" ]
                    (defs
                        ++ roads
                        ++ buildings
                    )
                )
            )
        ]
