module City.Park exposing (ParkType(..), forest, lawn)

import City.Tile as Tile exposing (Tile)
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox)


type ParkType
    = Forest
    | Lawn


forest : Tile -> Svg msg
forest tile =
    svg (Tile.attrs tile ++ [ viewBox "0 0 65.126 40.953" ])
        [ path [ d "M32.563 40.953L0 20.475 32.563 0l32.563 20.475z", fill "#d8dded" ]
            []
        , path [ d "M32.563 36.863L6.503 20.475l26.06-16.386 26.06 16.386z", fill "#72a041" ]
            []
        , path [ d "M35.006 35.795c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M36.267 34.605c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M38.12 33.502c0 .78-.83 1.411-1.853 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M39.492 32.356c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M42.326 31.782c0 .78-.83 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.829-1.41 1.852-1.41 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M43.737 30.371c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M45.589 28.74c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M47.132 27.55c0 .778-.83 1.41-1.852 1.41-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M49.558 26.49c0 .78-.83 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M51.233 25.08c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M53.262 24.021c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M54.938 22.61c0 .78-.83 1.411-1.852 1.411-1.023 0-1.853-.632-1.853-1.41 0-.78.83-1.412 1.853-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M57.451 21.684c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M58.642 20.476c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411M9.99 20.26c0 .78-.829 1.411-1.851 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M11.252 19.07c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M13.105 17.967c0 .78-.83 1.411-1.853 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.853.632 1.853 1.41", fill "#72a041" ]
            []
        , path [ d "M14.477 16.82c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M17.311 16.247c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M18.722 14.836c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M20.574 13.204c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M22.118 12.014c0 .779-.83 1.41-1.852 1.41-1.024 0-1.853-.631-1.853-1.41 0-.78.83-1.411 1.853-1.411 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M24.543 10.955c0 .78-.83 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.829-1.41 1.852-1.41 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M26.219 9.544c0 .78-.83 1.411-1.852 1.411-1.023 0-1.853-.631-1.853-1.41 0-.78.83-1.412 1.853-1.412 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M28.247 8.486c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M29.923 7.075c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M32.436 6.149c0 .78-.83 1.41-1.852 1.41-1.023 0-1.852-.63-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M33.627 4.941c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.022 0 1.852.632 1.852 1.411M11.667 21.2c0 .778-.83 1.41-1.853 1.41-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.853.632 1.853 1.411", fill "#72a041" ]
            []
        , path [ d "M13.519 22.862c0 .779-.83 1.41-1.852 1.41-1.023 0-1.853-.631-1.853-1.41 0-.78.83-1.411 1.853-1.411 1.022 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M15.746 24.021c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M17.598 26.138c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M20.266 26.844c0 .779-.83 1.41-1.853 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.853.632 1.853 1.412", fill "#72a041" ]
            []
        , path [ d "M22.426 28.255c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M24.278 29.313c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411M32.921 34.605c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M30.584 33.502c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411M26.219 30.945c0 .779-.83 1.41-1.852 1.41-1.023 0-1.853-.631-1.853-1.41 0-.78.83-1.412 1.853-1.412 1.022 0 1.852.632 1.852 1.412", fill "#72a041" ]
            []
        , path [ d "M28.732 32.091c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.022 0 1.852.632 1.852 1.411M58.978 20.273c0 .78-.829 1.411-1.852 1.411-1.022 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411M35.198 4.958c0 .78-.83 1.411-1.852 1.411-1.022 0-1.852-.632-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M36.874 5.897c0 .78-.83 1.411-1.852 1.411-1.023 0-1.853-.631-1.853-1.41 0-.78.83-1.412 1.853-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M38.726 7.56c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M40.953 8.72c0 .779-.829 1.41-1.852 1.41-1.022 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.412", fill "#72a041" ]
            []
        , path [ d "M42.805 10.836c0 .78-.829 1.411-1.852 1.411-1.022 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M45.473 11.542c0 .78-.83 1.41-1.852 1.41-1.023 0-1.853-.63-1.853-1.41 0-.78.83-1.411 1.853-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M47.633 12.953c0 .78-.829 1.411-1.852 1.411-1.022 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M49.485 14.011c0 .78-.829 1.411-1.852 1.411-1.022 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411M58.128 19.303c0 .78-.829 1.411-1.852 1.411-1.022 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M55.791 18.2c0 .78-.829 1.412-1.852 1.412-1.022 0-1.852-.632-1.852-1.412 0-.779.83-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41M51.426 15.643c0 .78-.83 1.41-1.852 1.41-1.023 0-1.852-.63-1.852-1.41 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41", fill "#72a041" ]
            []
        , path [ d "M53.94 16.79c0 .779-.83 1.41-1.853 1.41-1.022 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411", fill "#72a041" ]
            []
        , path [ d "M50.074 19.818c0 .495-.526.896-1.176.896-.65 0-1.176-.401-1.176-.896 0-.494.526-.895 1.176-.895.65 0 1.176.4 1.176.895M52.271 19.303c0 .356-.378.644-.845.644-.467 0-.845-.288-.845-.644 0-.356.378-.644.845-.644.467 0 .845.288.845.644M15.746 20.818c0 .803-.854 1.455-1.908 1.455-1.054 0-1.91-.652-1.91-1.455s.856-1.454 1.91-1.454c1.054 0 1.908.651 1.908 1.454M13.95 23.095c0 .356-.378.644-.845.644-.468 0-.846-.288-.846-.644 0-.355.378-.643.846-.643.467 0 .845.288.845.643M27.592 27.55c0 .299-.319.541-.712.541-.393 0-.712-.242-.712-.542 0-.3.32-.542.712-.542.393 0 .712.243.712.542M43.038 26.844c0 .3-.32.542-.712.542-.393 0-.712-.243-.712-.542 0-.3.32-.543.712-.543.393 0 .712.243.712.543M33.148 8.133c0 .3-.319.542-.712.542-.392 0-.711-.242-.711-.542 0-.299.319-.542.711-.542.393 0 .712.243.712.542M29.929 8.877c0 .514-.549.932-1.224.932-.676 0-1.224-.418-1.224-.932 0-.516.548-.933 1.224-.933.675 0 1.224.417 1.224.933", fill "#587b41" ]
            []
        ]


lawn : Tile -> Svg msg
lawn tile =
    svg (Tile.attrs tile ++ [ viewBox "0 0 52.119 33.118" ])
        [ path [ fill "#65a430", d "M26.06 32.774L0 16.386 26.06 0l26.06 16.386-26.06 16.388" ]
            []
        , path [ fill "#65a430", d "M28.5 31.707c0 .779-.828 1.41-1.851 1.41-1.02 0-1.852-.631-1.852-1.41 0-.78.832-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.412" ]
            []
        , path [ fill "#65a430", d "M29.764 30.516c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M31.616 29.414c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.412" ]
            []
        , path [ fill "#65a430", d "M32.988 28.267c0 .78-.829 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M35.821 27.694c0 .779-.829 1.41-1.852 1.41-1.02 0-1.852-.631-1.852-1.41 0-.78.832-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M37.232 26.283c0 .779-.829 1.41-1.852 1.41-1.02 0-1.852-.631-1.852-1.41 0-.78.833-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M39.084 24.651c0 .78-.829 1.411-1.852 1.411-1.02 0-1.852-.632-1.852-1.41 0-.78.833-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M40.63 23.46c0 .78-.83 1.412-1.853 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.83-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M43.053 22.402c0 .78-.829 1.411-1.852 1.411-1.02 0-1.852-.632-1.852-1.41 0-.78.832-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411M46.757 19.933c0 .779-.829 1.41-1.852 1.41-1.02 0-1.852-.631-1.852-1.41 0-.78.833-1.411 1.852-1.411 1.023 0 1.852.631 1.852 1.41M50.948 17.595c0 .78-.829 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.829-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41M10.81 12.158c0 .78-.83 1.411-1.853 1.411-1.023 0-1.852-.632-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M12.22 10.747c0 .78-.829 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M14.072 9.115c0 .78-.829 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.83-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M15.614 7.925c0 .78-.83 1.41-1.852 1.41-1.023 0-1.852-.63-1.852-1.41 0-.78.829-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M18.041 6.866c0 .78-.829 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.829-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M19.717 5.455c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411M5.165 17.11c0 .78-.83 1.412-1.852 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.829-1.41 1.852-1.41 1.023 0 1.852.631 1.852 1.41M11.095 22.05c0 .779-.83 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M13.762 22.755c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M15.924 24.166c0 .78-.829 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M17.776 25.224c0 .78-.829 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411M26.42 30.516c0 .78-.83 1.411-1.853 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.83-1.411 1.852-1.411 1.024 0 1.853.632 1.853 1.411" ]
            []
        , path [ fill "#65a430", d "M24.08 29.414c0 .779-.828 1.41-1.851 1.41-1.024 0-1.853-.631-1.853-1.41 0-.78.83-1.412 1.853-1.412 1.023 0 1.852.632 1.852 1.412M19.717 26.856c0 .78-.83 1.411-1.852 1.411-1.023 0-1.852-.632-1.852-1.411 0-.78.829-1.411 1.852-1.411 1.023 0 1.852.632 1.852 1.41" ]
            []
        , path [ fill "#65a430", d "M22.229 28.002c0 .78-.83 1.412-1.853 1.412-1.023 0-1.852-.632-1.852-1.412 0-.779.83-1.41 1.852-1.41 1.024 0 1.853.631 1.853 1.41M36.3 6.748c0 .779-.828 1.41-1.851 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.829-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.412" ]
            []
        , path [ fill "#65a430", d "M38.971 7.453c0 .78-.832 1.411-1.852 1.411-1.026 0-1.852-.632-1.852-1.41 0-.78.826-1.412 1.852-1.412 1.02 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M41.13 8.864c0 .78-.829 1.411-1.852 1.411-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.411" ]
            []
        , path [ fill "#65a430", d "M42.982 9.923c0 .779-.829 1.41-1.852 1.41-1.023 0-1.852-.631-1.852-1.41 0-.78.83-1.412 1.852-1.412 1.023 0 1.852.632 1.852 1.412" ]
            []
        , path [ fill "#4e7a38", d "M43.572 15.73c0 .494-.53.895-1.179.895s-1.174-.4-1.174-.895.525-.896 1.174-.896c.65 0 1.179.4 1.179.896M45.77 15.214c0 .355-.382.644-.847.644-.466 0-.847-.289-.847-.644 0-.355.381-.644.847-.644.465 0 .846.289.846.644M9.243 16.73c0 .803-.854 1.454-1.909 1.454-1.055 0-1.908-.651-1.908-1.455 0-.802.853-1.454 1.908-1.454s1.909.652 1.909 1.454M7.447 19.007c0 .355-.377.644-.847.644-.465 0-.843-.29-.843-.644 0-.356.378-.644.843-.644.47 0 .847.288.847.644M21.09 23.46c0 .3-.318.543-.714.543-.391 0-.712-.243-.712-.543 0-.299.32-.542.712-.542.396 0 .713.243.713.542M36.534 22.755c0 .3-.318.542-.713.542-.392 0-.709-.243-.709-.542 0-.3.317-.543.71-.543.394 0 .712.244.712.543M26.645 4.044c0 .3-.317.543-.712.543-.392 0-.713-.243-.713-.543 0-.299.321-.542.713-.542.395 0 .712.243.712.542M23.424 4.788c0 .515-.546.932-1.224.932-.673 0-1.22-.417-1.22-.932 0-.516.547-.933 1.22-.933.678 0 1.224.417 1.224.933" ]
            []
        ]