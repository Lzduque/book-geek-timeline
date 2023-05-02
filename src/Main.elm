module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Entry = String

type alias Book = 
  { name : String
  , number : String
  , year : String
  , month : String
  , entries : List Entry
  }

type alias Timeline =
 { bookSeriesName : String
  , books : List Book
  }
  
type alias Model = 
  { timeline : Timeline
   , newBookFormField : Book
   , newEntryFormField : { content : Entry, bookNumber : String }
  } 


init : Model
init =
  { timeline = initialTimeline
   , newBookFormField = initialBook
   , newEntryFormField = { content = initialEntry, bookNumber = "0" }
  } 


initialTimeline : Timeline
initialTimeline =
 { bookSeriesName = "Anita Blake"
  , books = [Book "Guilty Pleasures" "1" "0" "July" ["Nikolaos dies", "Jean-Claude becames Master of the City", "Anita receives the first and second marks"]]
  }

initialBook : Book
initialBook =
  { name = ""
  , number = "0"
  , year = "0"
  , month = "Jan"
  , entries = []
  }

initialEntry : Entry
initialEntry = ""

-- UPDATE


type Msg
  = NewBook
  | NewEntry
  | SetBookNumber String
  | SetBookName String
  | SetBookYear String
  | SetBookMonth String
  | SetBookEntryContent String
  | SetBookEntryNumber String


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewBook ->
      { model | timeline = addBook model.timeline model.newBookFormField }

    NewEntry ->
      { model | timeline = addEntry model.newEntryFormField.bookNumber model.newEntryFormField.content model.timeline }
    
    SetBookNumber number ->
      { model | newBookFormField = addBookNumber model.newBookFormField number }

    SetBookName name ->
      { model | newBookFormField = addBookName model.newBookFormField name }

    SetBookYear year ->
      { model | newBookFormField = addBookYear model.newBookFormField year }

    SetBookMonth month ->
      { model | newBookFormField = addBookMonth model.newBookFormField month }

    SetBookEntryContent content ->
      { model | newEntryFormField = addEntryContent model.newEntryFormField content }

    SetBookEntryNumber bookNumber ->
      { model | newEntryFormField = addEntryBookNumber model.newEntryFormField bookNumber }


addEntryBookNumber : { content : Entry, bookNumber : String } -> String -> { content : Entry, bookNumber : String }
addEntryBookNumber entry bookNumber =
    { entry | bookNumber = bookNumber}


addEntryContent : { content : Entry, bookNumber : String } -> String -> { content : Entry, bookNumber : String }
addEntryContent entry content =
    { entry | content = content}


addBookNumber : Book -> String -> Book
addBookNumber book number =
    { book | number = number}


addBookName : Book -> String -> Book
addBookName book name =
    { book | name = name}


addBookYear : Book -> String -> Book
addBookYear book year =
    { book | year = year}


addBookMonth : Book -> String -> Book
addBookMonth book month =
    { book | month = month}


addEntry : String -> Entry -> Timeline -> Timeline
addEntry bookNumber entry timeline =
    let findBook b = 
           if b.number == bookNumber then
            { b | entries = b.entries ++ [entry]}
           else b
    in { timeline | books = List.map findBook timeline.books}


addBook : Timeline -> Book -> Timeline
addBook timeline book =
    { timeline | books = timeline.books ++ [book]}


-- VIEW


view : Model -> Html Msg
view model =
  div []
        [ p [ style "text-align" "left" ]
            [ text "— "
            , text ("Timeline: " ++ model.timeline.bookSeriesName)
            ]
            , p [ style "text-align" "left" ]
            [ text "— "
            , text ("Books: ")
            ]
            , viewBook model.timeline.books
            , p [ style "text-align" "left" ]
            [ text "— "
            , text ("Add new Book: ")
            ]
            , Html.form [] 
                [ label []
                    [ text "Number"
                    , input [ type_ "text", name "number", onInput SetBookNumber ] []
                    ]
                , label []
                    [ text "Name"
                    , input [ type_ "text", name "name", onInput SetBookName  ] []
                    ]
                , label []
                    [ text "Year"
                    , input [ type_ "text", name "year", onInput SetBookYear  ] []
                    ]
                , label []
                    [ text "Month"
                    , input [ type_ "text", name "month" , onInput SetBookMonth  ] []
                    ]
                , button [ onClick NewBook ] [ text "Submit" ]
                ]
            , p [ style "text-align" "left" ]
            [ text "— Add New Entry: "]
            , Html.form [] 
                [ label []
                    [ text "Book Number"
                    , input [ type_ "text", name "bookNumber", onInput SetBookEntryNumber ] []
                    ]
                , label []
                    [ text "Content"
                    , input [ type_ "text", name "content", onInput SetBookEntryContent  ] []
                    ]
                , button [ onClick NewBook ] [ text "Submit" ]
                ]
            -- form : List (Attribute msg) -> List (Html msg) -> Html msg
            -- , p [ style "text-align" "left" ]
            -- [ text "— "
            -- , text ("Add new Entry: ")
            -- ] -- TODO
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
                        , text ("Number: " ++ b.number)
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Year: " ++ b.year)
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
                        , text e
                    ]
                    ]
    in div [] (List.map entryView entries)
