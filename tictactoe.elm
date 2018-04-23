import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Maybe.Extra exposing (values)


main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL
type Symbol = X | O
type alias Square = Maybe Symbol
type alias Board = Array (Square)

type alias Model =
  { board : Board
  , activePlayer: Symbol
  , message: String
  }

model = 
  { board = initialize 9 (always Nothing)
  , activePlayer = O
  , message = ""
  }

-- UPDATE

numberOfPossibleMoves : Board -> Int
numberOfPossibleMoves board =
  List.length <| List.filter ((==) Nothing) (toList board)

clearMessage : Model -> Model
clearMessage = setMessage ""

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
        | board = set index (Just model.activePlayer) model.board
        }
        |> switchPlayer

    -- filled cell
    Just _ -> 
      setMessage "You can't go there" model

    -- index out of range
    Nothing ->
      setMessage "Something went seriously wrong!" model
  

switchPlayer : Model -> Model
switchPlayer model =
  { model
  | activePlayer = case model.activePlayer of
      X -> O
      O -> X
  }


type Msg = PlaySquare Int

getSquares : List Int -> Board -> List Square
getSquares squares board = 
  Maybe.Extra.values <| List.map (\s -> Array.get s board) squares

full : List Square -> Bool
full squares = 
  case Maybe.Extra.values squares of 
    [a, b, c] ->
       a == b && b == c
    _ ->
      False


checkStatus : Model -> Model
checkStatus model =
  let
    rows =  [ toList (slice 0 3 model.board)
            , toList (slice 3 6 model.board)
            , toList (slice 6 9 model.board)
            ]
    columns =
      [ getSquares [0, 3, 6] model.board
      , getSquares [1, 4, 7] model.board
      , getSquares [2, 5, 8] model.board
      ]
    diagonals =
      [ getSquares [0, 4, 8] model.board
      , getSquares [2, 4, 6] model.board
      ]

    possibleWinner =
      -- We switch the active player when we fill the square
      -- so we need to switch it back for the winner
      case model.activePlayer of
        X -> O
        O -> X
       
  in
    -- check for a win
    if List.any full <| rows ++ columns ++ diagonals then
      setMessage ((toString possibleWinner) ++ " wins!") model
    -- check for a draw
    else if (numberOfPossibleMoves model.board) == 0 then
      setMessage "It's a draw!" model
    else
      model

update : Msg -> Model -> Model
update msg model =
  case msg of
    PlaySquare idx ->
      fillSquare idx model
        |> checkStatus

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
