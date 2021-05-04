module City.TileType exposing (TileType(..))

import City.Building exposing (BuildingType)
import City.Hospital exposing (HospitalType)
import City.Housing exposing (HousingType)
import City.Park exposing (ParkType)


type TileType
    = Building BuildingType
    | Park ParkType
    | Housing HousingType
    | Education
    | Hospital HospitalType
    | BlankTile
