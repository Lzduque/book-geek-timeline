module Main exposing (..)

import Browser
import List.Extra exposing (..)
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


type alias Entry = 
    { content : String
    , bookPosition : Position
    , year : Maybe Year
    -- , month : Maybe Month
    }

type Year = Year Int

-- type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec


-- months : List String
-- months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "None"]

type Position = Position Int

type Error = Error String

type alias Book = 
  { name : String
  , position : Position
  , colour : String
--   , year : Maybe Year
--   , month : Maybe Month
  }

type alias Timeline =
 { bookSeriesName : String
  , books : List Book
  , entries : List Entry
  }
  
type alias Model = 
  { timeline : Timeline
   , newBookFields : Book
   , newEntryFields : Entry
   , errorMessage : Maybe Error -- TODO: turn into maybe
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
    , newEntryFields = initialEntry
   , errorMessage = Nothing
  }

initialTimeline : Timeline
initialTimeline =
 { bookSeriesName = "Anita Blake"
  , books = [ Book "Guilty Pleasures" (Position 1) "lightpink" --(Just (Year 0)) --(Just Jul)
                , Book "The Laughing Corpse" (Position 2) "wheat" -- (Just (Year 0)) --(Just Aug)
                ]
    , entries = initialEntries
  }

initialBook : Book
initialBook =
  { name = ""
  , position = Position 1
  , colour = "lightgrey"
--   , year = Just (Year 0)
--   , month = Just Jan
  }


initialEntry : Entry
initialEntry = { content = ""
                        ,  bookPosition = Position 1
                        , year = Nothing
                        -- , month = Nothing
                    }


initialEntries : List Entry
initialEntries = [ { content = "Nikolaos dies"
                        ,  bookPosition = Position 1
                        , year = Just (Year 0)
                        -- , month = Just Jul
                    }
                    , { content = "Jean-Claude becames Master of the City"
                        ,  bookPosition = Position 1
                        , year = Just (Year 0)
                        -- , month = Just Jul
                    }
                    , { content = "Anita receives the first and second marks"
                        ,  bookPosition = Position 1
                        , year = Just (Year 0)
                        -- , month = Just Jul
                    }
                    , { content = "Anita meets vaudun priests Dominga Salvador"
                        ,  bookPosition = Position 2
                        , year = Just (Year 0)
                        -- , month = Just Jul
                    }
                    ]


-- UPDATE


type Msg
  = NewBook
  | NewEntry
  | SetBookPosition String
  | SetBookName String
  | SetBookColour String
  | SetBookEntryContent String
  | SetBookEntryPosition String
  | SetBookEntryYear String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    NewBook ->
      if hasPositionAlready model.timeline model.newBookFields then
       ( { model | errorMessage = Just (Error "Position is already taken!")}
        , Cmd.none
        )
      else 
        ({ model | timeline = addBook model.timeline model.newBookFields, errorMessage = Nothing } -- TODO: after sending the form the fields should be emptied again
        , Cmd.none
        )


    NewEntry ->
     ( { model | timeline = addEntry model.newEntryFields model.timeline, errorMessage = Nothing } -- TODO: after sending the form the fields should be emptied again
    --  TODO: it shouldn't send an empty string like entry
      , Cmd.none
      )
    
    SetBookPosition position ->
      let n = String.toInt position in
      ({ model | newBookFields = addBookPosition model.newBookFields (Position (Maybe.withDefault 0 n)) }
      , Cmd.none
      )

    SetBookName name ->
      ({ model | newBookFields = addBookName model.newBookFields name }
      , Cmd.none
      )

    SetBookColour colour ->
      ({ model | newBookFields = addBookColour model.newBookFields colour }
      , Cmd.none
      )

    -- SetBookYear year ->
    --   let y = String.toInt year in
    --   ({ model | newBookFields = addBookYear model.newBookFields (Maybe.withDefault 0 y) }
    --   , Cmd.none)

    -- SetBookMonth month ->
    --   ({ model | newBookFields = addBookMonth model.newBookFields month }
    --   , Cmd.none
    --   )

    SetBookEntryContent content ->
      ({ model | newEntryFields = addEntryContent model.newEntryFields content }
      , Cmd.none
      )

    SetBookEntryPosition bookPosition ->
      ({ model | newEntryFields = addEntryBookPosition model.newEntryFields bookPosition }
      , Cmd.none
      )

    SetBookEntryYear year ->
      ({ model | newEntryFields = addEntryBookYear model.newEntryFields year }
      , Cmd.none
      )


addEntryBookYear : Entry -> String -> Entry
addEntryBookYear entry year =
    let n = String.toInt year in
    case n of
        Just y -> { entry | year = Just (Year y)}
        Nothing -> { entry | year = Nothing}


addEntryBookPosition : Entry -> String -> Entry
addEntryBookPosition entry bookPosition =
    let n = String.toInt bookPosition in
    { entry | bookPosition = Position (Maybe.withDefault 0 n)}


addEntryContent : Entry -> String -> Entry
addEntryContent entry content =
    { entry | content = content}


addBookPosition : Book -> Position -> Book
addBookPosition book position =
    { book | position = position}


addBookName : Book -> String -> Book
addBookName book name =
    { book | name = name}


addBookColour : Book -> String -> Book
addBookColour book colour =
    { book | colour = colour}


-- addBookYear : Book -> Int -> Book
-- addBookYear book year =
--     { book | year = Just (Year year)}


-- addBookMonth : Book -> String -> Book
-- addBookMonth book month =
--     { book | month = getMonth month}


addEntry : Entry -> Timeline -> Timeline
addEntry entry timeline =
    if List.any (\b -> b.position == entry.bookPosition) timeline.books then
    { timeline | entries = timeline.entries ++ [entry] }
    else timeline



addBook : Timeline -> Book -> Timeline
addBook timeline book = -- TODO: make sure that the position is not repeated
    -- case hasPositionAlready timeline.books book.position of
    --     True ->
    --             timeline -- { model | errorMessage = Error "Position is already taken!"}
    --     False ->
    { timeline | books = timeline.books ++ [book]}


hasPositionAlready : Timeline -> Book -> Bool
hasPositionAlready timeline newBook=
    let
        books = timeline.books
        position = newBook.position
        isPosition b = b.position == position
    in
    List.length (List.filter isPosition books) > 0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


getYear : Maybe Year -> String
getYear year = 
    case year of
        Just (Year num) -> String.fromInt num
        Nothing -> ""


-- getMonthStr : Maybe Month -> String
-- getMonthStr month = 
--     case month of
--         Just Jan -> "Jan"
--         Just Feb -> "Feb"
--         Just Mar -> "Mar"
--         Just Apr -> "Apr"
--         Just May -> "May"
--         Just Jun -> "Jun"
--         Just Jul -> "Jul"
--         Just Aug -> "Aug"
--         Just Sep -> "Sep"
--         Just Oct -> "Oct"
--         Just Nov -> "Nov"
--         Just Dec -> "Dec"
--         Nothing -> "None"



-- getMonth : String -> Maybe Month
-- getMonth month = 
--     case month of
--         "Jan" -> Just Jan
--         "Feb" -> Just Feb
--         "Mar" -> Just Mar
--         "Apr" -> Just Apr
--         "May" -> Just May
--         "Jun" -> Just Jun
--         "Jul" -> Just Jul
--         "Aug" -> Just Aug
--         "Sep" -> Just Sep
--         "Oct" -> Just Oct
--         "Nov" -> Just Nov
--         "Dec" -> Just Dec
--         _ -> Nothing


monthToOption : String -> Html Msg
monthToOption v =
  option [ value v ] [ text v ]


getPosition : Position -> String
getPosition position = 
    case position of
        Position num -> String.fromInt num


getPositionNum : Position -> Int
getPositionNum position = 
    case position of
        Position num -> num


view : Model -> Html Msg
view model =
  div [ class "body"
        , style "display" "flex" 
        , style "flex-direction" "row" 
        , style "flex-wrap" "wrap" 
        , style "margin" "15px" 
        , style "row-gap" "10px"  
        , style "column-gap" "10px"  
        , style "justify-content" "flex-start" ]
        [ h1 [ style "text-align" "left"
                , style "weight" "bold"
                , style "width" "100%" ] -- header
                [ text (model.timeline.bookSeriesName)
                ]
            , div [ class "timeline"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "justify-content" "flex-start"
                ] [ viewEntries model.timeline.books model.timeline.entries, viewLegend model.timeline.books ]
            , div [ class "forms"
                    , style "display" "flex"
                    , style "flex-direction" "row"
                    , style "flex-wrap" "wrap"
                    , style "justify-content" "flex-start"
                    , style "row-gap" "10px"
                    , style "column-gap" "10px"
                    , style "align-items" "flex-start"
                    , style "align-content" "flex-start"
                    ] [ div [] [ viewNewBookForm model.errorMessage ]
                    , div [] [ viewNewEntryForm model.errorMessage ]
            ]
        ]


viewNewEntryForm : Maybe Error -> Html Msg
viewNewEntryForm error =
    div [ class "new-entry-form" 
            , style "display" "flex" 
            , style "flex-direction" "column"
            , style "padding" "0 10px 10px 10px"          
            , style "background-color" "lightgreen"
            , style "width" "auto"
            , style "max-width" "300px"
            ] [ p [ style "text-align" "left"
                    , style "font-weight" "bold" ]
                    [ text "Add new Entry: "]
                , div [ class "new-entry-inputs" 
                    , style "display" "inline-flex" 
                    , style "flex-direction" "column" 
                    , style "flex-wrap" "wrap" 
                    , style "margin-bottom" "10px" 
                    , style "align-content" "flex-start" 
                    , style "align-items" "flex-start" 
                    ]
                    [ label [ style "margin-right" "20px"
                                , style "margin" "5px" 
                                ]
                                [ text "Book Position: "
                                , input [ type_ "text"
                                            , name "bookPosition"
                                            , onInput SetBookEntryPosition
                                            , style "margin-left" "5px"
                                            , style "width" "50px" ] []
                                ]
                    , label [ style "margin-right" "20px"
                                , style "margin" "5px" 
                                ]
                                [ text "Book Year: "
                                , input [ type_ "text"
                                            , name "bookYear"
                                            , onInput SetBookEntryYear, style "margin-left" "5px"
                                            , style "width" "75px" ] []
                                ]
                    , label [ style "margin" "5px"  
                                , style "display" "flex" 
                                , style "flex-direction" "column" 
                                ]
                                [ text "Content: "
                                , input [ type_ "text"
                                            , name "content"
                                            , onInput SetBookEntryContent
                                            , style "width" "280px"] []
                                ]
                    , button [ onClick NewEntry, style "margin-left" "5px"  ] [ text "Submit" ]
                ]
            ]


viewNewBookForm : Maybe Error -> Html Msg
viewNewBookForm error =
    div [ class "new-book-form" 
            , style "display" "inline-flex" 
            , style "flex-direction" "column" 
            , style "margin-bottom" "10px"          
            , style "padding" "0 10px 10px 10px"          
            , style "background-color" "powderblue"
            , style "width" "auto"
            , style "max-width" "300px"
            ] [ p [ style "text-align" "left"
                    , style "font-weight" "bold" ]
                    [ text ("Add new Book: ")
                    ]
                , div [ class "new-book-inputs" 
                    , style "display" "inline-flex" 
                    , style "flex-direction" "column" 
                    , style "flex-wrap" "wrap" 
                    , style "margin-bottom" "10px" 
                    , style "align-content" "flex-start" 
                    , style "align-items" "flex-start" 
                    ]
                    [ label [ style "margin-right" "20px"
                                , style "margin" "5px" 
                                ]
                                [ text "Position: "
                                , input [ type_ "text"
                                            , name "position"
                                            , onInput SetBookPosition
                                            , style "margin-left" "5px"
                                            , style "width" "50px" ] []
                                ]
                    , label [ style "margin-right" "20px"
                                , style "margin" "5px" 
                                ]
                                [ text "Name: "
                                , input [ type_ "text"
                                            , name "name"
                                            , onInput SetBookName
                                            , style "margin-left" "5px"
                                            , style "width" "150px" ] []
                                ]
                    , label [ style "margin" "5px"   
                                , style "display" "flex" 
                                , style "flex-direction" "column" 
                                ]
                                [ text "Colour: "
                                , input [ type_ "text"
                                            , name "colour"
                                            , onInput SetBookColour
                                            , style "width" "200px" ] []
                                ]
                    , button [ onClick NewBook, style "margin-left" "5px" ] [ text "Submit" ]
                ]
            , div [] [ viewError error ] -- TODO: add new book ERROR
            ]


viewError : Maybe Error -> Html Msg
viewError error =
    case error of
        Nothing -> div [] []
        Just (Error e) -> p [ style "text-align" "left" ] [ text "— Error: " , text e] 


groupByPosition : List Entry -> List ( Entry, List Entry )
groupByPosition entries = List.Extra.groupWhile (\a b -> (getPositionNum a.bookPosition) == (getPositionNum b.bookPosition)) (List.sortBy (\x -> getPositionNum x.bookPosition) entries)
    

viewEntries : List Book -> List Entry -> Html msg
viewEntries books entries =
    let
        groupedEntries = groupByPosition entries
    in
    div [ class "entries"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "align-items" "stretch"
            ] (List.map (\(a, b) -> viewBookEntries books (a :: b)) groupedEntries
            ) -- sorted and grouped now by the book position, not year, as MVP


viewBookEntries : List Book -> List Entry -> Html msg
viewBookEntries books group =
    let
        bookPosition = (Maybe.withDefault initialEntry (List.head group)).bookPosition
        entryView e =
            let
             bookColour = (Maybe.withDefault initialBook (List.Extra.find (\b -> b.position == bookPosition) books)).colour
            in
            div []
                    [ p [  class "entry"
                            , style "height" "50px"
                            , style "border-bottom" "solid"
                            , style "border-width" "0.5px"
                            , style "margin" "0px"
                            , style "padding" "10px"
                            , style "display" "flex"
                            , style "justify-content" "flex-start"
                            , style "align-items" "center"
                            , style "background-color" bookColour
                            ] [ text e.content ]
                    ]
    in div [ class "position-entries"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center" 
                , style "justify-content" "flex-end" 
                ] [ div [ class "entries"
                            , style "border" "black"
                            , style "border-left" "solid"
                            , style "border-width" "0.5px"
                            , style "width" "200px"
                            , style "display" "flex"
                            , style "flex-direction" "column-reverse"
                        ] (List.map entryView group)
                        , p [ class "position" ] [ text (getPosition bookPosition) ]
            ]


viewLegend : List Book -> Html msg
viewLegend books =
    let 
        bookView b =
            div [ class "book-info"
                    , style "margin-bottom" "10px"
                    , style "margin-right" "10px"
                    , style "display" "flex"
                    , style "flex-direction" "row"
                    , style "text-align" "center"
                    , style "align-items" "stretch"
                    ]
                    [ div [ style "height" "20px"
                            , style "width" "20px"
                            , style "padding-right" "10px"
                            , style "background-color" b.colour
                            ] []
                            , p [ style "min-height" "20px"
                            , style "margin" "0px"
                            , style "padding-left" "10px"
                            , style "padding-right" "10px"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            ] [ text (getPosition b.position)]
                            , p [ style "min-height" "20px"
                            , style "margin" "0px"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            ] [ text b.name ]
            ]
    in div [ class "legends" ] 
                [ p [] [ text "Legends: "]
                    , div [ style "display" "flex"
                            , style "flex-direction" "row"
                            , style "flex-wrap" "wrap"
                            , style "justify-content" "flex-start"
                            ] (List.map bookView books)
                ]   
