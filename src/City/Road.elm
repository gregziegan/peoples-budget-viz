module City.Road exposing (Road, RoadType(..), def, view)

import City.Building exposing (BuildingType(..))
import City.Housing exposing (HousingType(..))
import City.Tile as Tile exposing (Rotation(..), Tile)
import City.TileType exposing (TileType(..))
import Svg exposing (Svg, defs, path, svg)
import Svg.Attributes as Attr exposing (d, fill, style, transform, viewBox, xlinkHref)


type RoadType
    = Corner -- West <-> South
    | Tee -- North <-> West <-> South intersection
    | Junction -- 4 way intersection
    | Straight { hasCrosswalk : Bool } --  North <-> South
    | Empty TileType


type alias Road =
    { style : RoadType
    , rotation : Rotation
    }


useAlt road =
    case road.rotation of
        RNone ->
            False

        RQuarter ->
            True

        RHalf ->
            False

        RThreeQuarters ->
            True


def : Road -> Svg msg
def road =
    case road.style of
        Corner ->
            if useAlt road then
                cornerAlt

            else
                corner

        Tee ->
            if useAlt road then
                teeAlt

            else
                tee

        Junction ->
            fourWayIntersection

        Straight { hasCrosswalk } ->
            if useAlt road then
                if hasCrosswalk then
                    crosswalkAlt

                else
                    straightAlt

            else if hasCrosswalk then
                crosswalk

            else
                straight

        Empty tileType ->
            Tile.def tileType


id : Road -> String
id road =
    case road.style of
        Corner ->
            if useAlt road then
                "cornerAlt"

            else
                "corner"

        Tee ->
            if useAlt road then
                "teeAlt"

            else
                "tee"

        Junction ->
            "fourWayIntersection"

        Straight { hasCrosswalk } ->
            if useAlt road then
                if hasCrosswalk then
                    "crosswalk-alt"

                else
                    "straight-alt"

            else if hasCrosswalk then
                "crosswalk"

            else
                "straight"

        Empty tileType ->
            Tile.id tileType


view : Tile -> Road -> Svg msg
view tile road =
    case road.style of
        Empty BlankTile ->
            svg (Tile.position tile)
                [ Svg.g []
                    [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                    ]
                ]

        Empty (Building Skyscraper) ->
            Svg.g
                [ Attr.transform "translate(0, -100)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 200 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Building SmallMultiuse) ->
            Svg.g
                [ Attr.transform "translate(0, -15)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 100 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Building DepartmentStore) ->
            Svg.g
                [ Attr.transform "translate(0, -15)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 100 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Building Office) ->
            Svg.g
                [ Attr.transform "translate(10, -10)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 100 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Building LargeOffice) ->
            Svg.g
                [ Attr.transform "translate(0, -15)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 100 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Building MediumMultiuse) ->
            Svg.g
                [ Attr.transform "translate(0, -15)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 100 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Park _) ->
            svg (Tile.position tile)
                [ Svg.g []
                    [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                    ]
                ]

        Empty (Housing House) ->
            Svg.g
                [ Attr.transform "translate(15, 10)"
                ]
                [ svg (Tile.position { tile | width = 75, height = 75 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Housing LargeHouse) ->
            Svg.g
                [ Attr.transform "translate(5, 15)"
                ]
                [ svg (Tile.position { tile | width = 75, height = 75 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty (Housing (TallApartment _)) ->
            Svg.g
                [ Attr.transform "translate(0, -40)"
                ]
                [ svg (Tile.position { tile | width = 100, height = 125 })
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        Empty _ ->
            Svg.g
                [ Attr.transform "translate(0, -30)"
                ]
                [ svg (Tile.position tile)
                    [ Svg.g []
                        [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                        ]
                    ]
                ]

        _ ->
            svg (Tile.position tile)
                [ Svg.g (Tile.rotate tile)
                    [ Svg.use [ xlinkHref ("#" ++ id road) ] []
                    ]
                , Svg.text_ [ Attr.x "100", Attr.y "100", Attr.fontSize "10", fill "red" ] [ Svg.text (String.fromInt tile.x ++ ", " ++ String.fromInt tile.y) ]
                ]


teeAlt : Svg msg
teeAlt =
    svg [ Attr.id (id <| Road Tee RQuarter), viewBox "0 0 65.126 40.953" ]
        [ path [ fill "#dcdff0", d "M32.565 40.953L0 20.476 32.565 0l32.561 20.476-32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M23.534 35.248l-14.5-9.068 23.467-14.77 14.496 9.066-23.463 14.772" ]
            []
        , path [ fill "#fff", d "M16.708 30.996l-.847-.563 23.463-14.79.85.561-23.466 14.792" ]
            []
        , path [ fill "#a3b7dd", d "M36.728 22.249L30 18.036l6.357-4.276 6.728 4.212-6.357 4.277" ]
            []
        , path [ fill "#a3b7dd", d "M36.728 20.912L30 16.698l2.516-1.538 6.727 4.213-2.515 1.539M32.106 25.62l-6.727-4.213 3.468-2.024 6.727 4.212-3.468 2.025M26.853 28.497l-6.727-4.214 2.515-1.538 6.728 4.213-2.516 1.539M22.232 31.638l-6.727-4.214 2.515-1.539 6.727 4.212-2.515 1.54" ]
            []
        , path [ fill "#a3b7dd", d "M33.704 18.417c1.64-1.022 3.302-2.05 4.76-3.323l-3.842-2.416-11.088-6.973-14.5 9.067 11.77 7.4 3.033 1.907c3.302-1.864 6.65-3.655 9.867-5.662M42.944 17.91L40.6 16.438c-.07.064-.14.13-.211.192-1.56 1.394-3.313 2.547-5.088 3.653-2.98 1.86-6.064 3.548-9.126 5.268l3.193 2.008 12.227 7.69 14.5-9.068-13.152-8.27" ]
            []
        , path [ fill "#fff", d "M48.422 30.996l.847-.563L16.708 9.957l-.847.562 32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M16.87 14.895l6.727-4.213-2.515-1.538-6.727 4.212 2.515 1.54M22.324 17.77l6.724-4.212-2.516-1.539-6.727 4.212 2.519 1.54M29.245 22.016l6.33-3.98-2.964-2.876-6.724 4.213 3.358 2.643M33.024 25.62l6.723-4.213-3.986-2.394-5.934 3.732 3.197 2.875M38.276 28.497l6.728-4.214-2.515-1.538-6.728 4.213 2.515 1.539M42.898 31.638l6.727-4.214-2.515-1.539-6.728 4.212 2.516 1.54" ]
            []
        ]


tee : Svg msg
tee =
    svg [ Attr.id (id <| Road Tee RNone), viewBox "0 0 65.126 40.952" ]
        [ path [ d "M32.561 40.952L0 20.475 32.561 0l32.565 20.475-32.565 20.477", fill "#dcdff0" ]
            []
        , path [ d "M23.53 35.247L9.031 26.18 41.596 5.704l14.496 9.066L23.53 35.247", fill "#a3b7dd" ]
            []
        , path [ d "M16.704 30.995l-.847-.563L48.42 9.957l.85.56-32.565 20.478", fill "#fff" ]
            []
        , path [ d "M48.256 14.895l-6.727-4.214 2.515-1.537 6.728 4.211-2.516 1.54m-5.45 2.875l-6.727-4.212 2.515-1.54 6.727 4.213-2.515 1.54m-6.078 3.139L30 16.699l2.516-1.538 6.727 4.211-2.515 1.54m-4.622 4.708l-6.727-4.213 3.464-2.025 6.728 4.212-3.465 2.026m-5.256 2.876l-6.728-4.214 2.516-1.538 6.727 4.213-2.515 1.539m-4.621 3.141L15.5 27.423l2.515-1.538 6.728 4.212-2.515 1.54", fill "#a3b7dd" ]
            []
        , path [ d "M33.7 18.416c1.641-1.021 3.303-2.05 4.763-3.323l-3.841-2.415L23.53 5.704 9.031 14.77 20.8 22.172l3.034 1.907c3.302-1.865 6.65-3.655 9.867-5.663", fill "#a3b7dd" ]
            []
        , path [ d "M35.147 22.598l.847-.563-19.29-12.078-.847.56 19.29 12.08", fill "#fff" ]
            []
        , path [ d "M16.866 14.895l6.728-4.214-2.516-1.537-6.727 4.211 2.515 1.54m5.454 2.875l6.728-4.212-2.516-1.54-6.727 4.213 2.515 1.54m6.922 4.244l6.329-3.98-2.96-2.875-6.728 4.211 3.359 2.644m3.778 3.605l6.727-4.213-3.986-2.394-5.937 3.731 3.196 2.876", fill "#a3b7dd" ]
            []
        ]


fourWayIntersection : Svg msg
fourWayIntersection =
    svg [ Attr.id (id <| Road Junction RNone), viewBox "0 0 65.126 40.953" ]
        [ path [ fill "#dcdff0", d "M32.565 40.953L0 20.476 32.565 0l32.561 20.476-32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M23.53 35.247L9.031 26.181 41.596 5.704l14.5 9.067L23.53 35.247" ]
            []
        , path [ fill "#fff", d "M16.704 30.995l-.847-.562L48.422 9.957l.847.562-32.565 20.476" ]
            []
        , path [ fill "#a3b7dd", d "M48.26 14.895l-6.727-4.213 2.515-1.538 6.727 4.212-2.515 1.54M42.806 17.771l-6.727-4.213 2.515-1.538 6.727 4.211-2.515 1.54M36.728 20.912L30 16.698l2.516-1.538 6.727 4.212-2.515 1.54M32.106 25.62l-6.727-4.213 3.468-2.024 6.727 4.212-3.468 2.025M26.85 28.496l-6.728-4.213 2.516-1.538 6.727 4.212-2.515 1.54M22.229 31.637L15.5 27.424l2.515-1.539 6.728 4.213-2.515 1.54" ]
            []
        , path [ fill "#a3b7dd", d "M33.704 18.417c1.637-1.022 3.299-2.05 4.76-3.323l-3.842-2.416L23.53 5.704 9.031 14.771l11.772 7.401 3.034 1.907c3.302-1.864 6.647-3.655 9.867-5.662M42.944 17.91l-2.346-1.473c-.07.064-.138.13-.208.192-1.56 1.394-3.313 2.547-5.088 3.653-2.984 1.86-6.064 3.548-9.13 5.268l3.193 2.008 12.231 7.69 14.5-9.067-13.152-8.27" ]
            []
        , path [ fill "#fff", d "M48.422 30.995l.847-.562L16.704 9.957l-.847.562 32.565 20.476" ]
            []
        , path [ fill "#a3b7dd", d "M16.87 14.895l6.724-4.213-2.516-1.538-6.723 4.212 2.515 1.54M22.32 17.771l6.728-4.213-2.516-1.538-6.727 4.211 2.515 1.54M29.245 22.017l6.33-3.981-2.964-2.876-6.728 4.212 3.362 2.645M33.02 25.62l6.727-4.213-3.986-2.394-5.937 3.732 3.196 2.875M38.276 28.496l6.728-4.213-2.515-1.538-6.728 4.212 2.515 1.54M42.898 31.637l6.727-4.213-2.515-1.539-6.728 4.213 2.516 1.54" ]
            []
        ]


corner : Svg msg
corner =
    svg [ Attr.id (id <| Road Corner RNone), viewBox "0 0 65.126 40.953" ]
        [ path [ fill "#dcdff0", d "M32.561 40.953l32.565-20.477L32.561 0 0 20.476l32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M32.498 29.544l14.5-9.068L23.53 5.706 9.031 14.77l23.467 14.773" ]
            []
        , path [ fill "#fff", d "M38.393 24.642l.85-.563L16.704 9.957l-.847.562 22.536 14.123" ]
            []
        , path [ fill "#a3b7dd", d "M16.866 14.895l6.728-4.213-2.516-1.538-6.727 4.212 2.515 1.54M22.32 17.771l6.728-4.212-2.516-1.54-6.727 4.213 2.515 1.54M28.399 20.912l6.727-4.214-2.515-1.538-6.728 4.212 2.516 1.54M33.514 26.249l5.44-5.125-3.468-2.024-6.728 4.212 4.756 2.937" ]
            []
        , path [ fill "#a3b7dd", d "M36.03 26.249l6.727-4.213-2.52-1.538-6.727 4.212 2.52 1.539" ]
            []
        , path [ fill "#a3b7dd", d "M22.183 17.91l2.342-1.473c.07.064.141.13.212.192 1.56 1.394 3.312 2.547 5.087 3.652 2.98 1.86 6.064 3.55 9.13 5.269l-3.193 2.008-12.23 7.69-14.5-9.067 13.152-8.27" ]
            []
        , path [ fill "#fff", d "M16.704 30.995l-.847-.562 19.908-12.678.846.561-19.907 12.68" ]
            []
        , path [ fill "#a3b7dd", d "M34.343 21.052l-6.329-3.98 2.963-2.876 6.728 4.213-3.362 2.643M32.12 25.842l-6.727-4.213 2.999-1.766 5.933 3.732-2.205 2.247M26.85 28.496l-6.728-4.213 2.516-1.538 6.727 4.212-2.515 1.54M22.229 31.637L15.5 27.424l2.515-1.539 6.728 4.213-2.515 1.54" ]
            []
        ]


cornerAlt : Svg msg
cornerAlt =
    svg [ Attr.id (id <| Road Corner RQuarter), viewBox "0 0 65.126 40.953" ]
        [ path [ d "M32.565 0L0 20.477l32.565 20.476 32.561-20.476L32.565 0", fill "#dcdff0" ]
            []
        , path [ d "M32.628 11.409L18.13 20.477l23.467 14.77 14.5-9.066-23.47-14.771", fill "#a3b7dd" ]
            []
        , path [ d "M26.734 16.31l-.85.563 22.538 14.122.847-.56L26.734 16.31", fill "#fff" ]
            []
        , path [ d "M48.26 26.058l-6.727 4.213 2.515 1.538 6.727-4.212-2.515-1.54m-5.454-2.876l-6.727 4.213 2.515 1.539 6.727-4.212-2.515-1.54m-6.078-3.14L30 24.255l2.516 1.537 6.727-4.212-2.515-1.539m-4.622-4.709l-6.727 4.214 3.468 2.024 6.727-4.212-3.468-2.026", fill "#a3b7dd" ]
            []
        , path [ d "M27.372 14.848l-5.468 5.443 5.302 3.04 6.728-4.213-6.562-4.27", fill "#a3b7dd" ]
            []
        , path [ d "M33.704 22.535c1.637 1.023 3.302 2.051 4.76 3.324l-3.842 2.416-11.088 6.973L9.03 26.18l11.772-7.4 3.034-1.908c3.302 1.864 6.65 3.656 9.867 5.662", fill "#a3b7dd" ]
            []
        , path [ d "M38.04 16.592l.847.563-22.18 13.84-.85-.56L38.04 16.591", fill "#fff" ]
            []
        , path [ d "M16.87 26.058l6.727 4.213-2.515 1.538-6.727-4.212 2.515-1.54m5.45-2.876l6.728 4.213-2.516 1.539-6.727-4.212 2.515-1.54m6.195-3.691l6.329 3.979-2.233 2.29-6.728-4.212 2.632-2.058m4.505-4.157l6.727 4.214-3.986 2.394-5.934-3.732 3.193-2.876", fill "#a3b7dd" ]
            []
        , path [ d "M36.195 14.539l6.727 4.214-2.515 1.538-6.727-4.212 2.515-1.54", fill "#a3b7dd" ]
            []
        ]


straight : Svg msg
straight =
    svg [ Attr.id (id <| Road (Straight { hasCrosswalk = False }) RNone), viewBox "0 0 65.123 40.951" ]
        [ path [ fill "#dcdff0", d "M32.561 40.951L0 20.476 32.561 0l32.562 20.476L32.56 40.95" ]
            []
        , path [ fill "#a3b7dd", d "M23.53 35.247L9.031 26.18 41.593 5.704l14.499 9.066L23.53 35.247" ]
            []
        , path [ fill "#fff", d "M16.704 30.995l-.847-.562L48.42 9.957l.846.561-32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M48.256 14.895l-6.727-4.213 2.515-1.538 6.728 4.211-2.516 1.54M42.806 17.771l-6.727-4.213 2.515-1.539 6.724 4.212-2.512 1.54M36.728 20.91L30 16.699l2.512-1.538 6.727 4.212-2.511 1.539M32.103 25.62l-6.728-4.213 2.516-1.539 6.727 4.213-2.515 1.54M26.85 28.496l-6.728-4.213 2.516-1.538 6.727 4.211-2.515 1.54M22.229 31.637L15.5 27.424l2.515-1.539 6.728 4.212-2.515 1.54" ]
            []
        ]


straightAlt : Svg msg
straightAlt =
    svg [ Attr.id (id <| Road (Straight { hasCrosswalk = False }) RQuarter), viewBox "0 0 65.126 40.953" ]
        [ path [ d "M32.565 40.953l32.561-20.477L32.565 0 0 20.476l32.565 20.477", fill "#dcdff0" ]
            []
        , path [ d "M41.596 35.247l14.5-9.066L23.53 5.705 9.031 14.771l32.565 20.476", fill "#a3b7dd" ]
            []
        , path [ d "M48.422 30.995l.847-.563L16.704 9.957l-.847.561 32.565 20.477", fill "#fff" ]
            []
        , path [ d "M16.866 14.895l6.728-4.213-2.516-1.538-6.727 4.212 2.515 1.539m5.454 2.876l6.728-4.213-2.516-1.538-6.727 4.212 2.515 1.54m6.079 3.14l6.727-4.214-2.515-1.538-6.728 4.212 2.516 1.54m4.621 4.708l6.727-4.213-2.515-1.538-6.727 4.212 2.515 1.54m5.256 2.875l6.728-4.213-2.515-1.539-6.728 4.212 2.515 1.54m4.622 3.141l6.727-4.213-2.515-1.538-6.728 4.212 2.516 1.54", fill "#a3b7dd" ]
            []
        ]


crosswalk : Svg msg
crosswalk =
    svg [ Attr.id (id <| Road (Straight { hasCrosswalk = True }) RNone), viewBox "0 0 65.126 40.953" ]
        [ path [ d "M32.565 40.953L0 20.476 32.565 0l32.561 20.476-32.561 20.477", fill "#dcdff0" ]
            []
        , path [ d "M23.53 35.247L9.035 26.181 41.596 5.705l14.5 9.066L23.53 35.247", fill "#a3b7dd" ]
            []
        , path [ d "M16.704 30.995l-.847-.562L48.422 9.957l.847.561-32.565 20.477", fill "#fff" ]
            []
        , path [ d "M48.26 14.895l-6.727-4.213 2.515-1.538 6.727 4.212-2.515 1.54m-5.454 2.875l-6.727-4.213 2.515-1.538 6.727 4.212-2.515 1.54", fill "#a3b7dd" ]
            []
        , path [ d "M33.88 23.883l-6.727-4.214 10.002-6.77 6.656 4.125-9.93 6.859", fill "#a3b7dd" ]
            []
        , path [ d "M32.106 25.62l-6.727-4.213 2.515-1.538 6.728 4.212-2.516 1.54m-5.256 2.875l-6.728-4.213 2.516-1.538 6.727 4.211-2.515 1.54m-4.621 3.141L15.5 27.424l2.515-1.538 6.728 4.212-2.515 1.54", fill "#a3b7dd" ]
            []
        , path [ d "M38.566 25.886l-1.704-1.254 8.466-5.47 1.704 1.253-8.466 5.47m-2.572-1.738l-1.704-1.253 8.467-5.471 1.704 1.253-8.467 5.471m-2.515-1.486l-1.704-1.254 8.47-5.47 1.704 1.254-8.47 5.47m-2.572-1.654l-1.704-1.254 8.467-5.471 1.704 1.254-8.467 5.47m-2.41-1.48l-1.704-1.255 8.47-5.47 1.705 1.253-8.47 5.472m-2.375-1.591L24.42 16.68l8.467-5.47 1.704 1.254-8.467 5.471", fill "#fff" ]
            []
        ]


crosswalkAlt : Svg msg
crosswalkAlt =
    svg [ Attr.id (id <| Road (Straight { hasCrosswalk = True }) RQuarter), viewBox "0 0 65.126 40.953" ]
        [ path [ fill "#dcdff0", d "M32.561 40.953l32.565-20.477L32.561 0 0 20.476l32.561 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M41.592 35.248l14.5-9.067L23.53 5.705 9.031 14.771l32.561 20.477" ]
            []
        , path [ fill "#fff", d "M48.422 30.995l.847-.562L16.704 9.957l-.847.561 32.565 20.477" ]
            []
        , path [ fill "#a3b7dd", d "M16.866 14.895l6.728-4.213-2.516-1.538-6.727 4.212 2.515 1.54m5.454 2.875l6.728-4.213-2.516-1.538-6.727 4.212 2.515 1.54" ]
            []
        , path [ fill "#a3b7dd", d "M31.246 23.882l6.727-4.213-10.001-6.77-6.657 4.125 9.93 6.858" ]
            []
        , path [ fill "#a3b7dd", d "M33.02 25.62l6.727-4.213-2.515-1.538-6.727 4.212 2.515 1.54m5.256 2.876l6.728-4.214-2.515-1.538-6.728 4.212 2.515 1.54m4.622 3.14l6.727-4.213-2.515-1.538-6.728 4.212 2.516 1.54" ]
            []
        , path [ fill "#fff", d "M26.56 25.886l1.705-1.255-8.467-5.47-1.704 1.254 8.467 5.47m2.571-1.737l1.704-1.255-8.466-5.47-1.704 1.253 8.466 5.472m2.516-1.487l1.704-1.254-8.47-5.47-1.705 1.253 8.47 5.471m2.573-1.654l1.703-1.255-8.47-5.47-1.704 1.254 8.47 5.47m2.41-1.481l1.704-1.254-8.47-5.47-1.704 1.253 8.47 5.471m2.374-1.59l1.704-1.255-8.467-5.47-1.704 1.254 8.467 5.47" ]
            []
        ]
