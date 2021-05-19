module City.Size exposing (CitySize(..), dimensions, fromWindow)


type CitySize
    = Small
    | Medium
    | Large


fromWindow : { width : number, height : number } -> CitySize
fromWindow window =
    if window.width < 600 || window.height < 800 then
        Small

    else if window.width < 1000 || window.height < 1000 then
        Medium

    else
        Large


dimensions : CitySize -> ( number, number )
dimensions size =
    case size of
        Small ->
            ( 8, 8 )

        Medium ->
            ( 12, 12 )

        Large ->
            ( 16, 16 )
