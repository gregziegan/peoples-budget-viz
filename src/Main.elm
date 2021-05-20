port module Main exposing (main, windowSize)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import City exposing (Budget, City, Range)
import City.Road exposing (Road, RoadType(..))
import City.Size exposing (CitySize(..))
import Color
import Data.Author as Author
import Date
import Element exposing (Device, DeviceClass(..), Element, Orientation(..), column, fill, padding, row, text, width)
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
-- main : Platform.Program Pages.Platform.Flags (Pages.Platform.City.generate model.citySize  Model Msg Metadata Rendered) (Pages.Platform.Msg Msg Metadata Rendered)


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
    , citySize : CitySize
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
                Tuple.first <| City.generate Small City.currentBudget randomSeed
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
      , citySize = Small
      }
    , Cmd.batch
        [ Task.perform InitializeRandomness Time.now
        ]
    )


type Msg
    = ChangedBudget BudgetArea Float
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


type BudgetArea
    = Police
    | Housing
    | Health
    | Transit
    | Parks


updateCurrent : Float -> Range -> Range
updateCurrent amount range =
    { range | current = amount }


addCurrent : Float -> Range -> Range
addCurrent amount range =
    { range | current = range.current + amount }


redistribute : Float -> Range -> Float
redistribute amount range =
    (amount - range.current) / -4


whileBalancing : BudgetArea -> Float -> Budget -> Budget
whileBalancing area amount budget =
    case area of
        Police ->
            let
                range =
                    budget.police

                distribute =
                    redistribute amount range
            in
            { budget
                | police = updateCurrent amount range
                , housing = addCurrent distribute budget.housing
                , health = addCurrent distribute budget.health
                , transit = addCurrent distribute budget.transit
                , parks = addCurrent distribute budget.parks
            }

        Housing ->
            let
                range =
                    budget.housing

                distribute =
                    redistribute amount range
            in
            { budget
                | housing =
                    updateCurrent amount range
                , police = addCurrent distribute budget.police
                , health = addCurrent distribute budget.health
                , transit = addCurrent distribute budget.transit
                , parks = addCurrent distribute budget.parks
            }

        Health ->
            let
                range =
                    budget.health

                distribute =
                    redistribute amount range
            in
            { budget
                | health = updateCurrent amount range
                , police = addCurrent distribute budget.police
                , housing = addCurrent distribute budget.housing
                , transit = addCurrent distribute budget.transit
                , parks = addCurrent distribute budget.parks
            }

        Transit ->
            let
                range =
                    budget.transit

                distribute =
                    redistribute amount range
            in
            { budget
                | transit = updateCurrent amount range
                , police = addCurrent distribute budget.police
                , housing = addCurrent distribute budget.housing
                , health = addCurrent distribute budget.health
                , parks = addCurrent distribute budget.parks
            }

        Parks ->
            let
                range =
                    budget.parks

                distribute =
                    redistribute amount range
            in
            { budget
                | parks = updateCurrent amount range
                , police = addCurrent distribute budget.police
                , housing = addCurrent distribute budget.housing
                , health = addCurrent distribute budget.health
                , transit = addCurrent distribute budget.transit
            }


updateBudget : BudgetArea -> Float -> Model -> ( Model, Cmd Msg )
updateBudget area amount model =
    ( { model
        | budget = whileBalancing area amount model.budget
        , city = City.generate model.citySize model.budget model.randomSeed |> Tuple.first
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
        ChangedBudget area value ->
            updateBudget area value model

        InitializeRandomness now ->
            let
                originalSeed =
                    Maybe.withDefault (Time.posixToMillis now) model.originalSeed

                seed =
                    Random.initialSeed <| originalSeed

                ( city, nextSeed ) =
                    City.generate model.citySize model.budget seed
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
                , city = Tuple.first <| City.generate model.citySize model.budget randomSeed
              }
            , Cmd.none
            )

        ChangedWindowSize window ->
            let
                newSize =
                    City.Size.fromWindow window

                newCity =
                    if newSize /= model.citySize then
                        City.generate newSize model.budget model.randomSeed
                            |> Tuple.first

                    else
                        model.city
            in
            ( { model
                | window = window
                , citySize = newSize
                , city = newCity
              }
            , Cmd.none
            )

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
                Layout.view model.window (pageView model siteMetadata page viewForPage) page
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
            , body = [ viewInteractiveCity model ]
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
    column
        [ Element.spacingXY 10 5
        , Element.paddingXY 10 40
        ]
        [ Element.text ("$" ++ (String.fromInt <| round <| range.min) ++ "M")
        , Input.slider
            [ Element.width (Element.px 50)
            , Element.height (Element.px 150)

            -- Here is where we're creating/styling the "track"
            , Element.behindContent
                (Element.el
                    [ Element.height fill
                    , Element.width (Element.px 2)
                    , Element.centerX
                    , Background.color Palette.color.secondary
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = onChange
            , label =
                Input.labelAbove
                    [ Element.alignTop
                    , Element.centerX
                    ]
                    (text label)
            , min = range.min
            , max = range.max
            , step = Just <| (range.max - range.min) / 5
            , value = range.current
            , thumb =
                Input.thumb (thumb ++ [ Element.below (Element.el [ Element.moveUp 20, Element.moveRight 20 ] (Element.text ("$" ++ (String.fromInt <| round <| range.current) ++ "M"))) ])
            }
        , Element.text ("$" ++ (String.fromInt <| round <| range.max) ++ "M")
        ]


viewEssentials : Budget -> Element Msg
viewEssentials { housing, transit, health, police, parks } =
    let
        children =
            [ [ viewSlider "Housing" (ChangedBudget Housing) housing ]
            , [ viewSlider "Transit" (ChangedBudget Transit) transit ]
            , [ viewSlider "Health" (ChangedBudget Health) health ]
            , [ viewSlider "Police" (ChangedBudget Police) police ]
            , [ viewSlider "Parks" (ChangedBudget Parks) parks ]
            ]
    in
    row [ width fill ]
        (List.map (column []) children)


viewTotals : Budget -> Element Msg
viewTotals budget =
    let
        originalTotal =
            City.total City.currentBudget

        total =
            City.total budget
    in
    Element.textColumn [ width fill ]
        [ Element.paragraph [ Element.centerX ] [ text ("2020 budget: $" ++ String.fromInt originalTotal ++ "M") ]
        , Element.paragraph
            ([ Element.centerX ]
                ++ (if originalTotal - total < 0 then
                        [ Font.color (Element.rgb 255 0 0) ]

                    else if originalTotal - total > 0 then
                        [ Font.color (Element.rgb 0 255 0) ]

                    else
                        []
                   )
            )
            [ text ("Your Budget: $" ++ String.fromInt total ++ "M")
            ]
        ]


buttonStyles =
    [ padding 10
    , Border.width 1
    , Border.rounded 3
    ]


wideControls : Budget -> Element Msg
wideControls budget =
    row
        [ width fill
        , Font.size 24
        ]
        [ column [ Element.alignLeft, Element.spacing 10 ]
            [ row [ Element.centerX ] [ viewEssentials budget ]
            ]
        , column [ Element.alignRight, Element.spacing 10 ]
            [ row [ Element.centerX ] [ viewTotals budget ] ]
        ]


narrowControls : Budget -> Element Msg
narrowControls budget =
    column
        [ width fill
        , Element.height fill
        , Element.spacing 2
        , Font.size 10
        ]
        [ row [ Element.alignLeft, Element.spacing 2 ]
            [ row [ Element.centerX ] [ viewEssentials budget ]
            ]
        , row [ Element.alignBottom, Element.spacing 2 ]
            [ row [ Element.centerX, Font.center ] [ viewTotals budget ] ]
        ]


mobileControls orientation budget =
    case orientation of
        Portrait ->
            narrowControls budget

        Landscape ->
            wideControls budget


controls device budget =
    case device.class of
        Phone ->
            mobileControls device.orientation budget

        Tablet ->
            mobileControls device.orientation budget

        Desktop ->
            wideControls budget

        BigDesktop ->
            wideControls budget


viewInteractiveCity : Model -> Element Msg
viewInteractiveCity model =
    let
        device =
            Element.classifyDevice model.window
    in
    column [ width fill, Element.spacing 10 ]
        [ row
            [ Element.centerX
            , Element.inFront (controls device model.budget)
            ]
            [ City.visualization device model.window model.city ]
        , row [ Element.centerX, Element.spacing 10, Element.padding 10 ]
            [ Input.button
                buttonStyles
                { onPress = Just NewCity, label = Element.text "Randomize" }
            , case model.originalSeed of
                Just originalSeed ->
                    Element.link (buttonStyles ++ [ Border.width 0, Background.color Palette.color.secondary, Font.color (Element.rgb255 255 255 255) ])
                        { url = "/?seed=" ++ String.fromInt originalSeed
                        , label =
                            Element.text "Share"
                        }

                Nothing ->
                    Element.none
            ]
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
