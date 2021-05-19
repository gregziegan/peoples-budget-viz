port module Main exposing (main, windowSize)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import City exposing (Budget, City, Range)
import City.Road exposing (Road, RoadType(..))
import Color
import Data.Author as Author
import Date
import Element exposing (Device, Element, column, fill, padding, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Feed
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Index
import Json.Decode
import Layout
import Markdown.Parser
import Markdown.Renderer
import Metadata exposing (Metadata, Window)
import MySitemap
import Page.Article
import Pages exposing (images, pages)
import Pages.ImagePath exposing (Dimensions)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath exposing (PagePath)
import Pages.Platform
import Pages.StaticHttp as StaticHttp
import Palette
import Random exposing (Generator, Seed)
import Task
import Tiler exposing (Board, Neighbor(..))
import Time exposing (Posix)
import Url
import Url.Parser
import Url.Parser.Query as Query exposing (Parser)


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Just Color.white
    , categories = [ Pages.Manifest.Category.education ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "A visualization for changes to the budget of Nashville."
    , iarcRatingId = Nothing
    , name = "peoples-budget-viz"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "peoples-budget-viz"
    , sourceIcon = images.logoPng
    , icons = []
    }


type alias Rendered =
    Element Msg



-- the intellij-elm plugin doesn't support type aliases for Programs so we need to use this line
-- main : Platform.Program Pages.Platform.Flags (Pages.Platform.Model Model Msg Metadata Rendered) (Pages.Platform.Msg Msg Metadata Rendered)


main : Pages.Platform.Program Model Msg Metadata Rendered Pages.PathKey
main =
    Pages.Platform.init
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , documents = [ markdownDocument ]
        , manifest = manifest
        , canonicalSiteUrl = canonicalSiteUrl
        , onPageChange = Just OnPageChange
        , internals = Pages.internals
        }
        |> Pages.Platform.withFileGenerator generateFiles
        |> Pages.Platform.toProgram


generateFiles :
    List
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        , body : String
        }
    ->
        StaticHttp.Request
            (List
                (Result
                    String
                    { path : List String
                    , content : String
                    }
                )
            )
generateFiles siteMetadata =
    StaticHttp.succeed
        [ Feed.fileToGenerate { siteTagline = siteTagline, siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        , MySitemap.build { siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        ]


markdownDocument : { extension : String, metadata : Json.Decode.Decoder Metadata, body : String -> Result error (Element msg) }
markdownDocument =
    { extension = "md"
    , metadata = Metadata.decoder
    , body =
        \markdownBody ->
            -- Html.div [] [ Markdown.toHtml [] markdownBody ]
            Markdown.Parser.parse markdownBody
                |> Result.withDefault []
                |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                |> Result.withDefault [ Html.text "" ]
                |> Html.div []
                |> Element.html
                |> List.singleton
                |> Element.paragraph [ Element.width Element.fill ]
                |> Ok
    }



---- INIT ----


type alias Model =
    { city : City
    , budget : City.Budget
    , originalSeed : Maybe Int
    , randomSeed : Seed
    , window : Metadata.Window
    }


init :
    Maybe
        { path :
            { path : PagePath Pages.PathKey
            , query : Maybe String
            , fragment : Maybe String
            }
        , metadata : metadata
        }
    -> ( Model, Cmd Msg )
init urlData =
    let
        seed =
            Maybe.withDefault 0 (Maybe.andThen extractSeed <| Maybe.map .path urlData)

        randomSeed =
            Random.initialSeed seed

        city =
            if seed == 0 then
                Tiler.emptyBoard

            else
                Tuple.first <| City.generate City.currentBudget randomSeed
    in
    ( { originalSeed =
            if seed == 0 then
                Nothing

            else
                Just seed
      , randomSeed = randomSeed
      , city = city
      , budget = City.currentBudget
      , window = { height = 0, width = 0 }
      }
    , Cmd.batch
        [ Task.perform InitializeRandomness Time.now
        ]
    )


type Msg
    = ChangedPoliceBudget Float
    | ChangedHousingBudget Float
    | ChangedTransitBudget Float
    | ChangedHealthBudget Float
    | ChangedParksBudget Float
    | InitializeRandomness Posix
    | Tick Float
    | OnPageChange
        { fragment : Maybe String
        , metadata : Metadata
        , path : PagePath Pages.PathKey
        , query : Maybe String
        }
    | ChangedWindowSize Window
    | NewCity


updateBudget : (Budget -> Budget) -> Model -> ( Model, Cmd Msg )
updateBudget transform model =
    ( { model
        | budget = transform model.budget
        , city = City.generate model.budget model.randomSeed |> Tuple.first
      }
    , Cmd.none
    )


extractSeed :
    { a
        | query : Maybe String
        , fragment : Maybe String
    }
    -> Maybe Int
extractSeed pageChange =
    case pageChange.query of
        Just queryStr ->
            let
                urlStr =
                    "https://example.com/?" ++ queryStr

                seed =
                    case Url.fromString urlStr of
                        Nothing ->
                            Nothing

                        Just url ->
                            Maybe.withDefault Nothing (Url.Parser.parse (Url.Parser.query (Query.int "seed")) url)
            in
            seed

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPoliceBudget value ->
            updateBudget
                (\budget ->
                    let
                        range =
                            budget.police
                    in
                    { budget | police = { range | current = value } }
                )
                model

        ChangedHousingBudget value ->
            updateBudget
                (\budget ->
                    let
                        range =
                            budget.housing
                    in
                    { budget | housing = { range | current = value } }
                )
                model

        ChangedTransitBudget value ->
            updateBudget
                (\budget ->
                    let
                        range =
                            budget.transit
                    in
                    { budget | transit = { range | current = value } }
                )
                model

        ChangedHealthBudget value ->
            updateBudget
                (\budget ->
                    let
                        range =
                            budget.health
                    in
                    { budget | health = { range | current = value } }
                )
                model

        ChangedParksBudget value ->
            updateBudget
                (\budget ->
                    let
                        range =
                            budget.parks
                    in
                    { budget | parks = { range | current = value } }
                )
                model

        InitializeRandomness now ->
            let
                originalSeed =
                    Maybe.withDefault (Time.posixToMillis now) model.originalSeed

                seed =
                    Random.initialSeed <| originalSeed

                ( city, nextSeed ) =
                    City.generate model.budget seed
            in
            ( { model
                | originalSeed = Just originalSeed
                , randomSeed = seed
                , city = city
              }
            , Cmd.none
            )

        Tick _ ->
            ( model, Cmd.none )

        OnPageChange page ->
            let
                seed =
                    extractSeed page

                randomSeed =
                    Maybe.withDefault model.randomSeed <| Maybe.map Random.initialSeed seed
            in
            ( { model
                | originalSeed = seed
                , randomSeed = randomSeed
                , city = Tuple.first <| City.generate model.budget randomSeed
              }
            , Cmd.none
            )

        ChangedWindowSize window ->
            ( { model | window = window }, Cmd.none )

        NewCity ->
            ( { model | originalSeed = Nothing }, Task.perform InitializeRandomness Time.now )



--subscriptions : Model -> Sub Msg


subscriptions _ _ _ =
    Sub.batch
        [ Browser.Events.onResize (\width height -> ChangedWindowSize (Window width height))
        , windowSize ChangedWindowSize
        ]


view :
    List ( PagePath Pages.PathKey, Metadata )
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    ->
        StaticHttp.Request
            { view : Model -> Rendered -> { title : String, body : Html Msg }
            , head : List (Head.Tag Pages.PathKey)
            }
view siteMetadata page =
    StaticHttp.succeed
        { view =
            \model viewForPage ->
                Layout.view (pageView model siteMetadata page viewForPage) page
        , head = head page.frontmatter
        }


pageView :
    Model
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Rendered
    -> { title : String, body : List (Element Msg) }
pageView model siteMetadata page viewForPage =
    case page.frontmatter of
        Metadata.Page metadata ->
            { title = metadata.title
            , body =
                [ viewForPage
                ]

            --        |> Element.textColumn
            --            [ Element.width Element.fill
            --            ]
            }

        Metadata.Article metadata ->
            Page.Article.view metadata viewForPage

        Metadata.Author author ->
            { title = author.name
            , body =
                [ Palette.blogHeading author.name
                , Author.view [] author
                , Element.paragraph [ Element.centerX, Font.center ] [ viewForPage ]
                ]
            }

        Metadata.BlogIndex ->
            { title = "People's Budget blog"
            , body =
                [ Element.column [ Element.padding 20, Element.centerX ] [ Index.view siteMetadata ]
                ]
            }

        Metadata.Visualization metadata ->
            { title = metadata.title
            , body = [ viewForPage, viewInteractiveCity model ]
            }


thumb =
    [ Element.width (Element.px 16)
    , Element.height (Element.px 16)
    , Border.rounded 8
    , Border.width 1
    , Border.color (Element.rgb 0.5 0.5 0.5)
    , Background.color (Element.rgb 1 1 1)
    ]


viewSlider : String -> (Float -> Msg) -> Range -> Element Msg
viewSlider label onChange range =
    row [ width (Element.px 400), Element.spacing 10, padding 20 ]
        [ Element.text ("$" ++ (String.fromInt <| round <| range.min) ++ "M")
        , Input.slider
            [ Element.height (Element.px 30)

            -- Here is where we're creating/styling the "track"
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color Palette.color.secondary
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = onChange
            , label =
                Input.labelAbove [ Element.centerX ]
                    (text label)
            , min = range.min
            , max = range.max
            , step = Just <| (range.max - range.min) / 5
            , value = range.current
            , thumb =
                Input.thumb (thumb ++ [ Element.below (Element.el [ padding 5 ] (Element.text ("$" ++ (String.fromInt <| round <| range.current) ++ "M"))) ])
            }
        , Element.text ("$" ++ (String.fromInt <| round <| range.max) ++ "M")
        ]


viewEssentials : Budget -> Element Msg
viewEssentials { housing, transit, health } =
    column [ width fill ]
        [ row [ Element.centerX ] [ viewSlider "Housing" ChangedHousingBudget housing ]
        , row [ Element.centerX ] [ viewSlider "Transit" ChangedTransitBudget transit ]
        , row [ Element.centerX ] [ viewSlider "Health" ChangedHealthBudget health ]
        ]


viewServices : Budget -> Element Msg
viewServices ({ police, parks } as budget) =
    let
        originalTotal =
            City.total City.currentBudget

        total =
            City.total budget
    in
    column [ width fill, Element.spacing 20 ]
        [ row [ Element.centerX ] [ viewSlider "Police" ChangedPoliceBudget police ]
        , row [ Element.centerX ] [ viewSlider "Parks" ChangedParksBudget parks ]
        , row [ Element.centerX ] [ text ("2020 Budget: $" ++ String.fromInt originalTotal ++ "M") ]
        , row
            ([ Element.centerX ]
                ++ (if originalTotal - total < 0 then
                        [ Font.color (Element.rgb 255 0 0) ]

                    else if originalTotal - total > 0 then
                        [ Font.color (Element.rgb 0 255 0) ]

                    else
                        []
                   )
            )
            [ text ("Your Budget: $" ++ String.fromInt total ++ "M") ]
        ]


buttonStyles =
    [ padding 10
    , Border.width 1
    , Border.rounded 3
    ]


viewInteractiveCity model =
    let
        device =
            Element.classifyDevice model.window
    in
    column [ width fill, Element.spacing 10 ]
        [ row [ Element.centerX, Element.spacing 10 ]
            [ Input.button
                buttonStyles
                { onPress = Just NewCity, label = Element.text "Generate another city" }
            , case model.originalSeed of
                Just originalSeed ->
                    Element.link (buttonStyles ++ [ Border.width 0, Background.color Palette.color.secondary, Font.color (Element.rgb255 255 255 255) ])
                        { url = "/?seed=" ++ String.fromInt originalSeed
                        , label =
                            Element.text "Share your city"
                        }

                Nothing ->
                    Element.none
            ]
        , row
            [ Element.centerX
            , Element.inFront
                (row
                    [ width fill
                    ]
                    [ column [ Element.alignLeft, Element.spacing 10 ]
                        [ row [ Element.centerX ] [ viewEssentials model.budget ]
                        ]
                    , column [ Element.alignRight, Element.spacing 10 ]
                        [ row [ Element.centerX ] [ viewServices model.budget ] ]
                    ]
                )
            ]
            [ City.visualization device model.window model.city ]
        ]


commonHeadTags : List (Head.Tag Pages.PathKey)
commonHeadTags =
    [ Head.rssLink "/blog/feed.xml"
    , Head.sitemapLink "/sitemap.xml"
    ]



{- Read more about the metadata specs:

   <https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/abouts-cards>
   <https://htmlhead.dev>
   <https://html.spec.whatwg.org/multipage/semantics.html#standard-metadata-names>
   <https://ogp.me/>
-}


head : Metadata -> List (Head.Tag Pages.PathKey)
head metadata =
    commonHeadTags
        ++ (case metadata of
                Metadata.Page meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "peoples-budget-viz"
                        , image =
                            { url = images.logoPng
                            , alt = "People's Budget logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.website

                Metadata.Article meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "People's Budget article"
                        , image =
                            { url = meta.image
                            , alt = meta.description
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.description
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.article
                            { tags = []
                            , section = Nothing
                            , publishedTime = Just (Date.toIsoString meta.published)
                            , modifiedTime = Nothing
                            , expirationTime = Nothing
                            }

                Metadata.Author meta ->
                    let
                        ( firstName, lastName ) =
                            case meta.name |> String.split " " of
                                [ first, last ] ->
                                    ( first, last )

                                [ first, middle, last ] ->
                                    ( first ++ " " ++ middle, last )

                                [] ->
                                    ( "", "" )

                                _ ->
                                    ( meta.name, "" )
                    in
                    Seo.summary
                        { canonicalUrlOverride = Nothing
                        , siteName = "peoples-budget-viz"
                        , image =
                            { url = meta.avatar
                            , alt = meta.name ++ "'s People's Budget articles."
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.bio
                        , locale = Nothing
                        , title = meta.name ++ "'s People's Budget articles."
                        }
                        |> Seo.profile
                            { firstName = firstName
                            , lastName = lastName
                            , username = Nothing
                            }

                Metadata.BlogIndex ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "People's Budget"
                        , image =
                            { url = images.logoPng
                            , alt = "People's Budget logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "People's Budget blog"
                        }
                        |> Seo.website

                Metadata.Visualization meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "peoples-budget-viz"
                        , image =
                            { url = images.logoPng
                            , alt = "People's Budget logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.website
           )


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://peoples-budget-viz.netlify.com"


siteTagline : String
siteTagline =
    "A coalition of grassroots Nashville organizations working with the local community on a democratic city budget."


port windowSize : (Window -> msg) -> Sub msg
