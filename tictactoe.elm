import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)


main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL
type Symbol = X | O
type alias Square = Maybe Symbol

type Row = List (Square)

type alias Model =
  { board : List (Row) }

model = {
  board = 
    [ [Just X, Nothing, Nothing ] 
    , [Nothing, Nothing, Nothing ] 
    , [Nothing, Nothing, Nothing ] 
    ]
  }

-- UPDATE

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model

    Decrement ->
      model


-- VIEW
view model =
  table [] [ tbody [] (List.map viewRow model.board) ]

tdStyle =
    style
        [ ( "width", "15vmin" )
        , ( "height", "15vmin" )
        , ( "font-size", "15vmin" )
        , ( "font-family", "monospace" )
        ]

viewRow row =
  tr [] (List.map viewSquare row)

viewSquare : Square -> Html Msg
viewSquare square = 
  let
      cell = case square of
        Nothing ->
          "-"
        Just X ->
          "x"
        Just O ->
          "o"
  in
    td [ tdStyle ] [text cell]
