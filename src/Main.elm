module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import City exposing (City)
import City.Road exposing (Road, RoadType(..))
import Color
import Data.Author as Author
import Date
import Element exposing (Element, column, fill, row, text, width)
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
import Metadata exposing (Metadata)
import MySitemap
import Page.Article
import Pages exposing (images, pages)
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
    , board : Board Road
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

        board =
            if seed == 0 then
                Tiler.emptyBoard

            else
                Tuple.first <| City.generate randomSeed
    in
    ( { originalSeed =
            if seed == 0 then
                Nothing

            else
                Just seed
      , randomSeed = randomSeed
      , board = board
      , city = City.init { parks = 1, parkingLots = 1, housing = 1 }
      , budget = City.initialBudget
      }
    , Cmd.batch
        [ Task.perform InitializeRandomness Time.now
        ]
    )


type Msg
    = ChangedParksBudget Float
    | InitializeRandomness Posix
    | Tick Float
    | OnPageChange
        { fragment : Maybe String
        , metadata : Metadata
        , path : PagePath Pages.PathKey
        , query : Maybe String
        }


updateBudget transform budget =
    transform budget


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
        ChangedParksBudget parksBudget ->
            let
                newBudget =
                    updateBudget (\budget -> { budget | parks = parksBudget }) model.budget
            in
            ( { model
                | budget = newBudget
                , city = City.changeBudget newBudget model.city
              }
            , Cmd.none
            )

        InitializeRandomness now ->
            let
                originalSeed =
                    Maybe.withDefault (Time.posixToMillis now) model.originalSeed

                seed =
                    Random.initialSeed <| originalSeed

                ( board, nextSeed ) =
                    City.generate seed
            in
            ( { model
                | originalSeed = Just originalSeed
                , randomSeed = nextSeed
                , board = board
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
                , board = Tuple.first <| City.generate randomSeed
              }
            , Cmd.none
            )



--subscriptions : Model -> Sub Msg


subscriptions _ _ _ =
    Sub.none


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
            { title = "elm-pages blog"
            , body =
                [ Element.column [ Element.padding 20, Element.centerX ] [ Index.view siteMetadata ]
                ]
            }

        Metadata.Visualization metadata ->
            { title = metadata.title
            , body = [ viewForPage, viewInteractiveCity model ]
            }


viewParksBudgetSlider : Model -> Element Msg
viewParksBudgetSlider model =
    Input.slider
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
        { onChange = ChangedParksBudget
        , label =
            Input.labelAbove []
                (text "Parks Budget (in millions of USD)")
        , min = 0
        , max = 10
        , step = Nothing
        , value = model.budget.parks
        , thumb =
            Input.defaultThumb
        }


viewInteractiveCity model =
    Element.column [ Element.width fill ]
        [ Element.row [ Element.centerX ] [ viewParksBudgetSlider model ]
        , case model.originalSeed of
            Just originalSeed ->
                Element.row [ Element.centerX ] [ text ("Seed: " ++ String.fromInt originalSeed) ]

            Nothing ->
                Element.none
        , Element.row [ Element.centerX ] [ City.visualization model.board model.city ]
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
                            , alt = "elm-pages logo"
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
                        , siteName = "elm-pages starter"
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
                            , alt = meta.name ++ "'s elm-pages articles."
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.bio
                        , locale = Nothing
                        , title = meta.name ++ "'s elm-pages articles."
                        }
                        |> Seo.profile
                            { firstName = firstName
                            , lastName = lastName
                            , username = Nothing
                            }

                Metadata.BlogIndex ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages"
                        , image =
                            { url = images.logoPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "elm-pages blog"
                        }
                        |> Seo.website

                Metadata.Visualization meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "peoples-budget-viz"
                        , image =
                            { url = images.logoPng
                            , alt = "elm-pages logo"
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
    "Starter blog for elm-pages"
