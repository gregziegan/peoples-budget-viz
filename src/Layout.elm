module Layout exposing (view)

import DocumentSvg
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Region
import Html exposing (Html)
import Metadata exposing (Metadata, Window)
import Pages
import Pages.Directory as Directory exposing (Directory)
import Pages.ImagePath as ImagePath
import Pages.PagePath as PagePath exposing (PagePath)
import Palette


view :
    Window
    -> { title : String, body : List (Element msg) }
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    -> { title : String, body : Html msg }
view window document page =
    { title = document.title
    , body =
        Element.column
            [ Element.width (Element.fill |> Element.maximum window.width) ]
            [ header window page.path
            , Element.column
                [ Element.padding 0
                , Element.spacing 40
                , Element.Region.mainContent
                , Element.width (Element.fill |> Element.maximum 800)
                , Element.centerX
                ]
                document.body
            ]
            |> Element.layout
                [ Element.width Element.fill
                , Font.size 20
                , Font.family [ Font.typeface "Roboto" ]
                , Font.color (Element.rgba255 0 0 0 0.8)
                ]
    }


header : Window -> PagePath Pages.PathKey -> Element msg
header window currentPath =
    Element.column [ Element.width Element.fill ]
        [ Element.el
            [ Element.height (Element.px 4)
            , Element.width Element.fill
            , Element.Background.gradient
                { angle = 0.2
                , steps =
                    [ Element.rgb255 0 242 96
                    , Element.rgb255 5 117 230
                    ]
                }
            ]
            Element.none
        , Element.row
            [ Element.paddingXY 25 4
            , Element.spaceEvenly
            , Element.width Element.fill
            , Element.Region.navigation
            , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.Border.color (Element.rgba255 40 80 40 0.4)
            ]
            [ Element.link [ Element.width Element.fill ]
                { url = "/"
                , label =
                    Element.row [ Element.width Element.fill, Font.size 30, Element.spacing 16 ]
                        [ DocumentSvg.view
                        , Element.textColumn [ Element.width Element.fill ]
                            (if window.width > 600 then
                                [ Element.text "Nashville People's Budget" ]

                             else
                                [ Element.paragraph [] [ Element.text "Nashville" ]
                                , Element.paragraph [] [ Element.text "People's" ]
                                , Element.paragraph [] [ Element.text "Budget" ]
                                ]
                            )
                        ]
                }
            ]
        ]


highlightableLink :
    PagePath Pages.PathKey
    -> Directory Pages.PathKey Directory.WithIndex
    -> String
    -> Element msg
highlightableLink currentPath linkDirectory displayName =
    let
        isHighlighted =
            currentPath |> Directory.includes linkDirectory
    in
    Element.link
        (if isHighlighted then
            [ Font.underline
            , Font.color Palette.color.primary
            ]

         else
            []
        )
        { url = linkDirectory |> Directory.indexPath |> PagePath.toString
        , label = Element.text displayName
        }
