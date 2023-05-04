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


type alias Entry = String -- TODO: make sure that this string is secure

type Year = Year Int -- TODO: make it into Maybe

type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec -- TODO: make it into Maybe


months : List String
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

type Position = Position Int

type Error = Error String -- TODO: make sure that this string is secure
-- TODO: make it into Maybe Error

type alias Book = 
  { name : String
  , position : Position
  , year : Year
  , month : Month
  , entries : List Entry
  }

type alias Timeline =
 { bookSeriesName : String -- TODO: make sure that this string is secure
  , books : List Book
  }
  
type alias Model = 
  { timeline : Timeline
   , newBookFields : Book
   , newEntryFields : { content : Entry, bookPosition : Position }
   , errorMessage : Error
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
    , newEntryFields = { content = initialEntry, bookPosition = Position 0 }
   , errorMessage = Error "" 
  }

initialTimeline : Timeline
initialTimeline =
 { bookSeriesName = "Anita Blake"
  , books = [Book "Guilty Pleasures" (Position 1) (Year 0) Jul ["Nikolaos dies", "Jean-Claude becames Master of the City", "Anita receives the first and second marks"]
  , Book "The Laughing Corpse" (Position 2) (Year 0) Aug []]
  }

initialBook : Book
initialBook =
  { name = ""
  , position = Position 0
  , year = Year 0
  , month = Jan
  , entries = []
  }

initialEntry : Entry
initialEntry = ""



-- UPDATE


type Msg
  = NewBook
  | NewEntry
  | SetBookPosition String
  | SetBookName String
  | SetBookYear String
  | SetBookMonth String
  | SetBookEntryContent String
  | SetBookEntryPosition String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    NewBook -> -- TODO: make sure that the position is not repeated
      if hasPositionAlready model.timeline model.newBookFields then
       ( { model | errorMessage = Error "Position is already taken!"}
        , Cmd.none
        )
      else 
        ({ model | timeline = addBook model.timeline model.newBookFields, errorMessage = Error "" }
        , Cmd.none
        )
            

    NewEntry ->
     ( { model | timeline = addEntry (model.newEntryFields.bookPosition) model.newEntryFields.content model.timeline, errorMessage = Error ""  }
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

    SetBookEntryPosition bookPosition ->
      ({ model | newEntryFields = addEntryBookPosition model.newEntryFields bookPosition }
      , Cmd.none
      )


addEntryBookPosition : { content : Entry, bookPosition : Position } -> String -> { content : Entry, bookPosition : Position }
addEntryBookPosition entry bookPosition =
    let n = String.toInt bookPosition in
    { entry | bookPosition = Position (Maybe.withDefault 0 n)}


addEntryContent : { content : Entry, bookPosition : Position } -> String -> { content : Entry, bookPosition : Position }
addEntryContent entry content =
    { entry | content = content}


addBookPosition : Book -> Position -> Book
addBookPosition book position =
    { book | position = position}


addBookName : Book -> String -> Book
addBookName book name =
    { book | name = name}


addBookYear : Book -> Int -> Book
addBookYear book year =
    { book | year = Year year}


addBookMonth : Book -> String -> Book
addBookMonth book month =
    { book | month = getMonth month}


addEntry : Position -> Entry -> Timeline -> Timeline
addEntry bookPosition entry timeline =
    let findBook b =  -- TODO: make sure that the string is secure
           if b.position == bookPosition then
            { b | entries = b.entries ++ [entry]}
           else b
    in { timeline | books = List.map findBook timeline.books}


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


getYear : Year -> String
getYear year = 
    case year of
        Year num -> String.fromInt num


getMonthStr : Month -> String
getMonthStr month = 
    case month of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"



getMonth : String -> Month
getMonth month = 
    case month of
        "Jan" -> Jan
        "Feb" -> Feb
        "Mar" -> Mar
        "Apr" -> Apr
        "May" -> May
        "Jun" -> Jun
        "Jul" -> Jul
        "Aug" -> Aug
        "Sep" -> Sep
        "Oct" -> Oct
        "Nov" -> Nov
        "Dec" -> Dec
        _ -> Debug.todo "branch '_' not implemented"


monthToOption : String -> Html Msg
monthToOption v =
  option [ value v ] [ text v ]


getPosition : Position -> String
getPosition position = 
    case position of
        Position num -> String.fromInt num


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
                    [ text "Position"
                    , input [ type_ "text", name "position", onInput SetBookPosition ] []
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
                    , Html.select [ name "month" , onInput SetBookMonth  ] (List.map monthToOption months)
                    ]
                , button [ onClick NewBook ] [ text "Submit" ]
                ]
            , div [] [ viewError model.errorMessage]
            , p [ style "text-align" "left" ]
            [ text "— Add New Entry: "]
            , div [] 
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


viewError : Error -> Html Msg
viewError error =
    case error of
        Error "" -> div [] []
        Error _ -> p [ style "text-align" "left" ] [ text "— Error: " , text (getError error)] 


getError : Error -> String
getError error = 
    case error of
        Error x -> x


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
                        , text ("Position: " ++ getPosition (b.position))
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Year: " ++ getYear b.year)
                    ]
                    , p [ style "text-align" "left" ]
                        [ text "—— "
                        , text ("Month: " ++ getMonthStr b.month)
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
