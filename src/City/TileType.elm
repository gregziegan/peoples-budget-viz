module City.TileType exposing (TileType(..))

import City.Housing exposing (HousingType)
import City.Park exposing (ParkType)
import City.Road exposing (RoadType)


type TileType
    = Skyscraper
    | Park ParkType
    | Housing HousingType
    | Road RoadType
    | BlankTile
