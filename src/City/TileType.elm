module City.TileType exposing (TileType(..))

import City.Building exposing (BuildingType)
import City.Housing exposing (HousingType)
import City.Park exposing (ParkType)
import City.Road exposing (RoadType)


type TileType
    = Building BuildingType
    | Park ParkType
    | Housing HousingType
    | Road RoadType
    | BlankTile
