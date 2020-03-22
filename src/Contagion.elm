module Contagion exposing (..)

import Array
import Playground exposing (..)


-- Model parameters.
-- TODO: make these parameters configurable.


-- Disease parameters.

infectedToOutcomeDays = 12   -- Days from the day of infection to the final outcome.
pInfection = 0.05            -- Probability of infection.
pDeath = 0.0346              -- Probability of death given infection.

-- Animation parameters.

patient0 : Position
patient0 = (gridHeight - 1, gridWidth // 2)

-- Maps game ticks to virtual days.
-- If the speed is X, then it takes X game ticks to simulate 1 virtual day.
animationSpeed : Int
animationSpeed = 10

-- Graphics parameters.

gridWidth = 50
gridHeight = 40

cellSize = 15
cellPad = 5

lineThickness = 1

lineColour = rgb 200 200 200

borderColour = rgb 165 30 20
backgroundColour = rgb 15 35 35


-- PRNG


type alias PRNG =
  { value : Int
  }

type alias Seed = Int

maxPRNG : Int
maxPRNG = 32767

newPRNG : Int -> PRNG
newPRNG seed = { value = seed }

stepPRNG : PRNG -> (Int, PRNG)
stepPRNG prng =
  let nextValue = modBy maxPRNG (8253729 * prng.value + 2396403)
  in (nextValue, { prng | value = nextValue })

uniformFloat : PRNG -> (Float, PRNG)
uniformFloat prng =
  let (value, newPrng) = stepPRNG prng
  in (toFloat value / toFloat maxPRNG, newPrng)

evalProbability : Probability -> Float -> Bool
evalProbability p x = x < p

-- The following two functions would ideally work on a "Probabilistic" class of
-- types, but Elm does not seem to have type classes.

-- Apply a function on a cell with a given probability.
withProbability : Probability -> (Cell -> Cell) -> Cell -> Cell
withProbability p updateCell = withProbability2 p updateCell identity

-- Apply the first function with the given probability p,
-- second function with probability (1-p).
withProbability2 : Probability -> (Cell -> Cell) -> (Cell -> Cell) -> Cell -> Cell
withProbability2 p updateOnTrue updateOnFalse cell =
  let (randFloat, nextPRNG) = uniformFloat cell.prng
      updateCell = if evalProbability p randFloat then updateOnTrue else updateOnFalse
      updated = updateCell cell
  in { updated | prng = nextPRNG }


-- Array2


type alias Width = Int
type alias Height = Int

type alias Array2 a =
  { data   : Array.Array a
  , width  : Width
  , height : Height
  }

makeArray2 : Width -> Height -> a -> Array2 a
makeArray2 width height val =
  let data = Array.initialize (width * height) (\x -> val)
  in { data = data, width = width, height = height }

toList2 : Array2 a -> List a
toList2 array2 = Array.toList array2.data

idx2 array2 row col = (row * array2.width) + col
get2 (row, col) array2 = Array.get (idx2 array2 row col) array2.data
set2 (row, col) val array2 = { array2 | data = Array.set (idx2 array2 row col) val array2.data }
modify2 f pos array2 =
  case get2 pos array2 of
    Nothing -> array2
    Just val -> set2 pos (f val) array2

map2 : (a -> b) -> Array2 a -> Array2 b
map2 f array2 = { data = Array.map f array2.data, width = array2.width, height = array2.height }

indexedMap2 : ((Int, Int) -> a -> b) -> Array2 a -> Array2 b
indexedMap2 f array2 =
  let
    makeRow idx1 = idx1 // array2.width
    makeCol idx1 = modBy array2.width idx1
    f1 = \idx a -> f (makeRow idx, makeCol idx) a
  in { data = Array.indexedMap f1 array2.data, width = array2.width, height = array2.height }


-- MODEL


type alias Probability = Float

type HealthStatus
  = Healthy
  | Dead
  | Infected
  { days : Int
  }
  | Recovered -- For now, this means you can't be re-infected.

type alias Cell =
  { healthStatus : HealthStatus
  , prng : PRNG
  }

healthyCell = { healthStatus = Healthy, prng = newPRNG 0 }

makeSick cell = { cell | healthStatus = Infected { days = 0 }}
killCell cell = { cell | healthStatus = Dead }
recoverCell cell = { cell | healthStatus = Recovered }

type alias Grid = Array2 Cell
type alias Position = (Int, Int)

newGrid : Width -> Height -> Grid
newGrid width height = makeArray2 width height healthyCell

randomizeGrid : PRNG -> Grid -> Grid
randomizeGrid prng grid =
  let randomize (row, col) cell = { cell | prng = cellPRNG row col }
      cellPRNG row col = newPRNG (modBy 876290 (row + 18793123) * modBy 107762 (col + 1998772))
  in indexedMap2 randomize grid

type alias AnimationSpeed = Int

type alias GameState =
  { grid : Grid
  , day  : Int
  , animationSpeed : AnimationSpeed
  , prng : PRNG
  }

type alias Patient = Position

newGame : AnimationSpeed -> Width -> Height -> GameState
newGame speed width height =
  { grid = newGrid width height
  , day = 0
  , animationSpeed = speed
  , prng = newPRNG 0
  }

seedGame : Seed -> Patient -> GameState -> GameState
seedGame seed patientZero game =
  { game
  | grid = modify2 makeSick patientZero (randomizeGrid (newPRNG (seed + 17622107)) game.grid)
  , prng = newPRNG (seed + 8162397)
  }


-- MAIN


main =
  let gamestate = newGame animationSpeed gridWidth gridHeight
  in game view update gamestate


-- VIEW


view computer gamestate =
  let
    w = computer.screen.width
    h = computer.screen.height
    b = computer.screen.bottom
    t = computer.screen.top
  in
  [ rectangle backgroundColour w h
  , rectangle borderColour w 100 |> moveY b
  , rectangle borderColour w 100 |> moveY t
  , drawGrid gamestate.grid
  ]

drawGrid : Grid -> Shape
drawGrid grid =
  let dx = toFloat (-grid.width * cellSize) / 2
      dy = toFloat (-grid.height * cellSize) / 2
  in move dx dy (Playground.group
  [ drawCells grid
  , drawHorizLines grid
  , drawVertLines grid
  ])

drawCells : Grid -> Shape
drawCells grid = Playground.group (toList2 (indexedMap2 drawCell grid))

drawCell : (Int, Int) -> Cell -> Shape
drawCell (row, col) cell =
  let cs = toFloat cellSize
      cp = toFloat cellPad
      x = (toFloat col * cs) + (cs / 2)
      y = (toFloat row * cs) + (cs / 2)
  in move x y (square (cellColour cell) (cs - cp))

cellColour : Cell -> Color
cellColour cell = case cell.healthStatus of
  Healthy     -> rgb 50 215 100
  Dead        -> rgb 30 30 30
  Infected {} -> rgb 165 30 20
  Recovered   -> rgb 50 135 90

drawHorizLines : Grid -> Shape
drawHorizLines grid =
  let lines = List.map drawLine (List.range 0 (grid.height))
      drawLine i = move (xoffset i) (yoffset i) (rectangle lineColour lineWidth lineHeight)
      xoffset i = lineWidth / 2
      yoffset i = toFloat (i * cellSize)
      lineWidth = toFloat (grid.width * cellSize)
      lineHeight = toFloat lineThickness
  in Playground.group lines

drawVertLines : Grid -> Shape
drawVertLines grid =
  let lines = List.map drawLine (List.range 0 (grid.width))
      drawLine i = move (xoffset i) (yoffset i) (rectangle lineColour lineWidth lineHeight)
      xoffset i = toFloat (i * cellSize)
      yoffset i = lineHeight / 2
      lineWidth = toFloat lineThickness
      lineHeight = toFloat (grid.height * cellSize)
  in Playground.group lines


-- UPDATE


update : Computer -> GameState -> GameState
update computer game =
    if game.day == 0
    then
      -- Poor man's randomization.
      let seed = modBy 9923716 (round ((computer.mouse.x * computer.screen.height * 1772366) +
                                       (computer.mouse.y * computer.screen.width * 63892)))
          seeded = seedGame seed patient0 game
      in { seeded | day = 1 }
    else updateGame game

updateGame : GameState -> GameState
updateGame game =
  let step = if modBy game.animationSpeed game.day == 0 then stepGame else identity
      updatedGame = step game
  in { updatedGame | day = game.day + 1 }

stepGame : GameState -> GameState
stepGame = spreadInfection << updateInfected

updateInfected : GameState -> GameState
updateInfected game = { game | grid = map2 (finalJudgement << stepInfected) game.grid }

finalJudgement : Cell -> Cell
finalJudgement cell =
  case cell.healthStatus of
    Infected { days } ->
      if days == infectedToOutcomeDays
      -- Make Healthy instead of Recovered to simulate re-infection.
      then withProbability2 pDeath killCell recoverCell cell
      else cell
    _ -> cell

stepInfected : Cell -> Cell
stepInfected cell =
  case cell.healthStatus of
    Infected { days } -> { cell | healthStatus = Infected { days = days + 1 }}
    _ -> cell

spreadInfection : GameState -> GameState
spreadInfection game = { game | grid = indexedMap2 (catchInfection game) game.grid }

catchInfection : GameState -> (Int, Int) -> Cell -> Cell
catchInfection game position cell
  = case cell.healthStatus of
      Healthy ->
        if neighboursInfected game.grid position
        then withProbability pInfection makeSick cell
        else cell
      _ -> cell -- Already infected, dead, or recovered.

neighboursInfected : Grid -> (Int, Int) -> Bool
neighboursInfected grid position =
  let sick neighbourPosition =
        case get2 neighbourPosition grid of
          Just cell ->
            case cell.healthStatus of
              Infected {} -> True
              _ -> False
          Nothing -> False
  in List.any sick (neighbours position)

neighbours : (Int, Int) -> List (Int, Int)
neighbours (row, col) =
  [ (row, col+1)
  , (row, col-1)
  , (row+1, col)
  , (row-1, col)
  , (row+1, col+1)
  , (row+1, col-1)
  , (row-1, col+1)
  , (row-1, col-1)
  ]


