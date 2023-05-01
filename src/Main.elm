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
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
