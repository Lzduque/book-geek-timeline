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
--   , colour : String
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
  , books = [ Book "Guilty Pleasures" (Position 1) --(Just (Year 0)) --(Just Jul)
                , Book "The Laughing Corpse" (Position 2)-- (Just (Year 0)) --(Just Aug)
                ]
    , entries = initialEntries
  }

initialBook : Book
initialBook =
  { name = ""
  , position = Position 1
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
--   | SetBookYear String
--   | SetBookMonth String
  | SetBookEntryContent String
  | SetBookEntryPosition String


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

    -- SetBookEntryYear year ->
    --   ({ model | newEntryFields = addEntryBookPosition model.newEntryFields bookPosition }
    --   , Cmd.none
    --   )


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
  div []
        [ p [ style "text-align" "left" ] -- header
            [ text "— "
            , text ("Timeline: " ++ model.timeline.bookSeriesName)
            ]
            , p [ style "text-align" "left" ]
                [ text "— "
                , text ("Books: ")
                ]
            , div [ class "timeline"
                , style "display" "flex"
                , style "flex-direction" "row"
                , style "align-items" "flex-end"
                ] [viewLabels, viewTimeLine model.timeline] -- timeline
            , p [ style "text-align" "left" ]
                [ text "— "
                , text ("Add new Book: ")
                ] -- add new book
            , div []  -- add new book
                [ label []
                    [ text "Position"
                    , input [ type_ "text", name "position", onInput SetBookPosition ] []
                    ]
                , label []
                    [ text "Name"
                    , input [ type_ "text", name "name", onInput SetBookName  ] []
                    ]
                -- , label []
                --     [ text "Year"
                --     , input [ type_ "text", name "year", onInput SetBookYear  ] []
                --     ]
                -- , label []
                --     [ text "Month"
                --     , Html.select [ name "month" , onInput SetBookMonth  ] (List.map monthToOption months)
                --     ]
                , button [ onClick NewBook ] [ text "Submit" ]
                ]
            , div [] [ viewError model.errorMessage] -- add new book ERROR
            , p [ style "text-align" "left" ]
                [ text "— Add New Entry: "] -- add new entry
            , div []  -- add new entry
                [ label []
                    [ text "Book Position"
                    , input [ type_ "text", name "bookPosition", onInput SetBookEntryPosition ] []
                    ]
                , label []
                    [ text "Content"
                    , input [ type_ "text", name "content", onInput SetBookEntryContent  ] []
                    ]
                , button [ onClick NewEntry ] [ text "Submit" ]
                ]
        ]


viewLabels : Html Msg
viewLabels =
    div [ class "labels"
            ,  style "display" "flex"
            , style "flex-direction" "column"
            ] [ div [ class "labels-entries"
                        , style "background-color" "lightgrey"
                        , style "width" "100px"
                        , style "display" "flex"
                        , style "flex-direction" "column-reverse"
                        ]
                        [ p [ style "height" "50px"
                            -- , style "border" "solid"
                            , style "border-bottom" "solid"
                            , style "border-right" "solid"
                            , style "border-width" "0.5px"
                            , style "margin" "0px"
                            , style "padding" "10px"
                            , style "display" "flex"
                            , style "justify-content" "flex-start"
                            , style "align-items" "center"
                            ]
                            [ text "Entries" ]
                        ]
                , div [ class "labels-book-info"
                        , style "background-color" "lightgrey"
                        , style "width" "100px"
                        , style "display" "flex"
                        , style "flex-direction" "column"
                        ]
                            [ p [ style "height" "50px"
                                -- , style "border" "solid"
                                , style "border-top" "solid"
                                , style "border-right" "solid"
                                , style "border-width" "0.5px"
                                , style "margin" "0px"
                                , style "padding" "10px"
                                , style "background-color" "plum"
                                , style "display" "flex"
                                , style "justify-content" "flex-start"
                                , style "align-items" "center"
                                ]
                                [ text "Book" ]
                        , p [ style "height" "50px"
                                -- , style "border" "solid"
                                , style "border-top" "solid"
                                , style "border-right" "solid"
                                , style "border-width" "0.5px"
                                , style "margin" "0px"
                                , style "padding" "10px"
                                , style "background-color" "pink"
                                , style "display" "flex"
                                , style "justify-content" "flex-start"
                                , style "align-items" "center"
                                ] [ text "Position" ]
                        -- , p [ style "height" "50px"
                        --         -- , style "border" "solid"
                        --         , style "border-top" "solid"
                        --         , style "border-right" "solid"
                        --         , style "border-width" "0.5px"
                        --         , style "margin" "0px"
                        --         , style "padding" "10px"
                        --         , style "background-color" "lightskyblue"
                        --         , style "display" "flex"
                        --         , style "justify-content" "flex-start"
                        --         , style "align-items" "center"
                        --         ] [ text "Year" ]
                        -- , p [ style "height" "50px"
                        --         -- , style "border" "solid"
                        --         , style "border-top" "solid"
                        --         , style "border-right" "solid"
                        --         , style "border-width" "0.5px"
                        --         , style "margin" "0px"
                        --         , style "padding" "10px"
                        --         , style "background-color" "lightgoldenrodyellow"
                        --         , style "display" "flex"
                        --         , style "justify-content" "flex-start"
                        --         , style "align-items" "center"
                        --         ] [ text "Month" ]
                        ]
            ]


viewError : Maybe Error -> Html Msg
viewError error =
    case error of
        Nothing -> div [] []
        Just (Error e) -> p [ style "text-align" "left" ] [ text "— Error: " , text e] 


-- each book should be a column, but rows should be uniform size
viewTimeLine : Timeline -> Html Msg
viewTimeLine timeline =
    div [  class "books"
            , style "display" "flex"
            , style "flex-direction" "column"
            ] [ viewEntries timeline.entries, viewBookInfo timeline.books ]

type alias GroupedEntriesBy a = 
    { position : a
    , entries : List Entry
    }

groupByPosition : List Entry -> List ( Entry, List Entry )
groupByPosition entries = List.Extra.gatherEqualsBy .bookPosition entries
    

viewEntries : List Entry -> Html msg
viewEntries entries =
    div [ class "books-entries"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "align-items" "stretch"
            ] (List.map viewEntry (groupByPosition entries)
            ) -- sorted and grouped now by the book position, not year


viewEntry : ( Entry, List Entry ) -> Html msg
viewEntry group =
    let
        entryView e =
            div []
                    [ p [  class "entry"
                            , style "height" "50px"
                            -- , style "border" "solid"
                            , style "border-bottom" "solid"
                            -- , style "border-left" "solid"
                            , style "border-width" "0.5px"
                            , style "margin" "0px"
                            , style "padding" "10px"
                            , style "display" "flex"
                            , style "justify-content" "flex-start"
                            , style "align-items" "center"
                            ] [ text e.content ]
                    ]
    in div [ class "entries"
                , style "background-color" "lightgrey"
                , style "border" "black"
                , style "border-left" "solid"
                , style "border-width" "0.5px"
                , style "width" "200px"
                , style "display" "flex"
                , style "flex-direction" "column-reverse"
                ] (List.map entryView ((Tuple.first  group) :: (Tuple.second group)))


viewBookInfo : List Book -> Html msg
viewBookInfo books =
    let 
        bookView b =
            div [ class "book-info"
                    , style "background-color" "lightgrey"
                    , style "border" "black"
                    , style "border-left" "solid"
                    , style "border-width" "0.5px"
                    , style "width" "200px"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "text-align" "center"
                    , style "align-items" "stretch"
                    ]
                    [ p [ style "height" "50px"
                            -- , style "border" "solid"
                            , style "border-top" "solid"
                            -- , style "border-left" "solid"
                            , style "border-width" "0.5px"
                            , style "margin" "0px"
                            , style "padding" "10px"
                            , style "background-color" "plum"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            ] [ text b.name ]
                    , p [ style "height" "50px"
                            -- , style "border" "solid"
                            , style "border-top" "solid"
                            -- , style "border-left" "solid"
                            , style "border-width" "0.5px"
                            , style "margin" "0px"
                            , style "padding" "10px"
                            , style "background-color" "pink"
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "align-items" "center"
                            ] [ text (getPosition b.position)]
                    -- , p [ style "height" "50px"
                    --         -- , style "border" "solid"
                    --         , style "border-top" "solid"
                    --         -- , style "border-left" "solid"
                    --         , style "border-width" "0.5px"
                    --         , style "margin" "0px"
                    --         , style "padding" "10px"
                    --         , style "background-color" "lightskyblue"
                    --         , style "display" "flex"
                    --         , style "justify-content" "center"
                    --         , style "align-items" "center"
                    --         ] [ text (getYear b.year) ]
                    -- , p [ style "height" "50px"
                    --         -- , style "border" "solid"
                    --         , style "border-top" "solid"
                    --         -- , style "border-left" "solid"
                    --         , style "border-width" "0.5px"
                    --         , style "margin" "0px"
                    --         , style "padding" "10px"
                    --         , style "background-color" "lightgoldenrodyellow"
                    --         , style "display" "flex"
                    --         , style "justify-content" "center"
                    --         , style "align-items" "center"
                    --         ] [ text (getMonthStr b.month)]
            ]
    in div [ class "books-infos"
                , style "display" "flex"
                , style "flex-direction" "row"] (List.map bookView books)
