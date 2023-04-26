module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.element { init = init, update = update, view = view }




-- MODEL



type Model
	= Maybe Timeline
	-- = Home (Maybe Timeline)
    -- | Redirect Session
    -- | NotFound Session
    -- | Home Home.Model
    -- | Settings Settings.Model
    -- | Login Login.Model
    -- | Register Register.Model
    -- | Profile Username Profile.Model
    -- | Article Article.Model
    -- | Editor (Maybe Slug) Editor.Model

type alias Timeline =
	{ bookSeriesName: String
	, books: List Book
	}

type alias Book =
	{ name: String
	, order: Int
	, year: Int
	, month: String
	, entries: Entry
	}

type alias Entry =
	{ tags: List String
	, content: String
	}

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing
  , Cmd.none
  )



-- UPDATE



type Msg
  = AddNewBook NewBook
  | AddNewEntry NewEntry
  | RefreshTimeline Timeline


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddNewBook newBook->
      ( model
      , newBook
      )

    AddNewEntry newEntry ->
      ( model
      , newEntry
      )

    RefreshTimeline timeline ->
      ( Model timeline
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
      [ div [ style "font-size" "12em" ] [ text (viewDie model.dieFace1) ] 
      , div [ style "font-size" "12em" ] [ text (viewDie model.dieFace2) ] 
      , button [ onClick Roll ] [ text "Roll" ]
      ]
