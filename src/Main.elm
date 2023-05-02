module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Entry = String

type alias Book = 
  { name : String
  , number : Int
  , year : Int
  , month : String
  , entries : List Entry
  }

type alias Timeline =
 { bookSeriesName : String
  , books : List Book
  }
  
type alias Model = 
  { timeline : Timeline
   , newBookFields : Book
   , newEntryFields : { content : Entry, bookNumber : Int }
  } 


init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none
  )


initialModel : Model
initialModel =
 { timeline = initialTimeline
    , newBookFields = initialBook
    , newEntryFields = { content = initialEntry, bookNumber = 0 }
  }

initialTimeline : Timeline
initialTimeline =
 { bookSeriesName = "Anita Blake"
  , books = [Book "Guilty Pleasures" 1 0 "July" ["Nikolaos dies", "Jean-Claude becames Master of the City", "Anita receives the first and second marks"]
  , Book "The Laughing Corpse" 2 0 "August" []]
  }

initialBook : Book
initialBook =
  { name = ""
  , number = 0
  , year = 0
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    NewBook ->
      ({ model | timeline = addBook model.timeline model.newBookFields }
      , Cmd.none
      )

    NewEntry ->
     ( { model | timeline = addEntry (model.newEntryFields.bookNumber) model.newEntryFields.content model.timeline }
      , Cmd.none
      )
    
    SetBookNumber number ->
      let n = String.toInt number in
      ({ model | newBookFields = addBookNumber model.newBookFields (Maybe.withDefault 0 n) }
      , Cmd.none
      )

    SetBookName name ->
      ({ model | newBookFields = addBookName model.newBookFields name }
      , Cmd.none
      )

    SetBookYear year ->
      let y = String.toInt year in
      ({ model | newBookFields = addBookYear model.newBookFields (Maybe.withDefault 0 y) }
      , Cmd.none)

    SetBookMonth month ->
      ({ model | newBookFields = addBookMonth model.newBookFields month }
      , Cmd.none
      )

    SetBookEntryContent content ->
      ({ model | newEntryFields = addEntryContent model.newEntryFields content }
      , Cmd.none
      )

    SetBookEntryNumber bookNumber ->
      ({ model | newEntryFields = addEntryBookNumber model.newEntryFields bookNumber }
      , Cmd.none
      )


addEntryBookNumber : { content : Entry, bookNumber : Int } -> String -> { content : Entry, bookNumber : Int }
addEntryBookNumber entry bookNumber =
    let n = String.toInt bookNumber in
    { entry | bookNumber = Maybe.withDefault 0 n}


addEntryContent : { content : Entry, bookNumber : Int } -> String -> { content : Entry, bookNumber : Int }
addEntryContent entry content =
    { entry | content = content}


addBookNumber : Book -> Int -> Book
addBookNumber book number =
    { book | number = number}


addBookName : Book -> String -> Book
addBookName book name =
    { book | name = name}


addBookYear : Book -> Int -> Book
addBookYear book year =
    { book | year = year}


addBookMonth : Book -> String -> Book
addBookMonth book month =
    { book | month = month}


addEntry : Int -> Entry -> Timeline -> Timeline
addEntry bookNumber entry timeline =
    let findBook b = 
           if b.number == bookNumber then
            { b | entries = b.entries ++ [entry]}
           else b
    in { timeline | books = List.map findBook timeline.books}


addBook : Timeline -> Book -> Timeline
addBook timeline book =
    { timeline | books = timeline.books ++ [book]}


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



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
            , div [] 
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
            , div [] 
                [ label []
                    [ text "Book Number"
                    , input [ type_ "text", name "bookNumber", onInput SetBookEntryNumber ] []
                    ]
                , label []
                    [ text "Content"
                    , input [ type_ "text", name "content", onInput SetBookEntryContent  ] []
                    ]
                , button [ onClick NewEntry ] [ text "Submit" ]
                ]
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
                        , text ("Number: " ++ (String.fromInt b.number))
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
                        , text e
                    ]
                    ]
    in div [] (List.map entryView entries)
