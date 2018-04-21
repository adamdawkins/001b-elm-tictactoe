import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)


main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL
type Symbol = X | O
type alias Square = Maybe Symbol
type alias Board = Array (Square)

type alias Model =
  { board : Board
  , nextTurn: Symbol
  , message: String
  }

model = 
  { board = initialize 9 (always Nothing)
  , nextTurn = O
  , message = ""
  }

-- UPDATE
clearMessage : Model -> Model
clearMessage model =
  { model
  | message = ""
  }

setMessage : String -> Model -> Model
setMessage message model =
  { model
  | message = message
  }


-- fill the square at index with the right Symbol and return the new Board
fillSquare : Int -> Model -> Model
fillSquare index model =
  case get index model.board of
    -- empty cell
    Just Nothing -> 
      clearMessage
        { model
        | board = set index (Just model.nextTurn) model.board
        }
        |> setNextTurn
    -- filled cell
    Just _ -> 
      setMessage "You can't go there" model
    -- index out of range
    Nothing ->
      setMessage "Something went seriously wrong!" model
  

setNextTurn : Model -> Model
setNextTurn model =
  { model
  | nextTurn = case model.nextTurn of
      X -> O
      O -> X
  }


type Msg = PlaySquare Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    PlaySquare idx ->
      fillSquare idx model


-- VIEW
view : Model -> Html Msg
view model =
  let rows =
        [ toList (slice 0 3 model.board)
        , toList (slice 3 6 model.board)
        , toList (slice 6 9 model.board)
        ]
  in
    div []
    [ h1 [] [ text model.message ]
    , table [] [ tbody [] (List.indexedMap viewRow rows) ]
    ]

tdStyle =
    style
        [ ( "width", "15vmin" )
        , ( "height", "15vmin" )
        , ( "font-size", "15vmin" )
        , ( "font-family", "monospace" )
        ]

viewRow : Int -> List Square -> Html Msg
viewRow index row =
  tr [] (List.indexedMap (viewSquare index) row)

viewSquare : Int -> Int -> Square -> Html Msg
viewSquare row col square = 
  let
      index = (row * 3) + col
      -- 0x1 0x2 0x3
      -- 0x1

      cell = case square of
        Nothing ->
          "-"
        Just X ->
          "x"
        Just O ->
          "o"
  in
    td [ tdStyle, onClick (PlaySquare index) ] [text cell]
