module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Entry =
  {content: String}

type alias Book = 
  { name: String
  , order: Int
  , year: Int
  , month: String
  , entries: List Entry
  }

type alias Timeline =
 { bookSeriesName : String
  , books : List Book
  }
  
type alias Model = Timeline 


init : Model
init =
  Timeline "Anita Blake" [Book "Guilty Pleasures" 1 0 "July" [Entry "Nikolaos dies", Entry "Jean-Claude becames Master of the City", Entry "Anita receives the first and second marks"]]


-- UPDATE


type Msg
  = NewBook Book
  | NewEntry Int Entry


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewBook book ->
      { model | books = model.books ++ [book] }

    NewEntry bookNumber entry ->
      { model | books = addEntry bookNumber entry model.books }


addEntry : Int -> Entry -> List Book -> List Book
addEntry bookNumber entry books =
    let findBook b = 
           if b.order == bookNumber then
            { b | entries = b.entries ++ [entry]}
           else b
    in List.map findBook books


-- VIEW


view : Model -> Html Msg
view model =
  div []
        [ p [ style "text-align" "left" ]
            [ text "— "
            , text ("Timeline: " ++ model.bookSeriesName)
            ]
            , p [ style "text-align" "left" ]
            [ text "— "
            , text ("Books: ")
            ]
            , viewBook model.books
        ]


viewBook : List Book -> Html Msg
viewBook books =
    let 
        bookView b =
            div []
                    [ p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Book: " ++ b.name)
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Order: " ++ (String.fromInt b.order))
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Year: " ++ (String.fromInt b.year))
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Month: " ++ b.month)
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Entries: ")
                    ]
                    , p [ style "text-align" "left" ]
                        [ viewEntries b.entries
                    ]
                    ]
    in div [] (List.map bookView books)


viewEntries : List Entry -> Html msg
viewEntries entries =
    let
        entryView e =
            div []
                    [ p [ style "text-align" "left" ]
                        [ text "——— "
                        , text e.content
                    ]
                    ]
    in div [] (List.map entryView entries)
