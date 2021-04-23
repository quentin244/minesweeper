module Main exposing (..)

import Mine
import Browser
import Html exposing (Html, div, h1, img, text, button, input, br)
import Html.Attributes exposing (src, style, placeholder)
import Html.Events exposing (onClick, onInput, on, stopPropagationOn, preventDefaultOn)
import Random exposing (int)
import Json.Decode as Decode

type alias Cell =
    { id : Int, isMine : Bool, nbBombAround : Int, revealed : Bool, isFlag:Bool }

type alias Model =
    { cells : List Cell, nbBomb : Int, gameOver : Bool, nbCellNotRevelead : Int}

exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 9
        , height = 9
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

init : ( Model, Cmd Msg )
init =
    ( { cells =
        List.map (\id -> { id = id, isMine = False, nbBombAround = 0, revealed = False, isFlag = False })
            (List.range 0 99), nbBomb = 0, gameOver = False, nbCellNotRevelead = 100 }, exampleGenerateRandomMines )

type Msg 
    = MinesGenerated (List ( Int, Int ))
    | RevealCell Cell
    | AddFlag Cell
    | LoadSave String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.gameOver == False && model.nbCellNotRevelead > 0 then
        case msg of
        LoadSave newModel ->
            (model, Cmd.none)
        MinesGenerated mines ->
            let newModel = updateMines model mines in
                (updateNbBombAround newModel, Cmd.none)
        RevealCell cell ->
            if cell.isFlag == False then
                if cell.isMine == False then
                    (revalAdjacentCell cell.id model, Cmd.none)
                else
                    (revalMine model, Cmd.none)
            else
                (model, Cmd.none)
        AddFlag cell ->
            if cell.revealed == False then
                if cell.isFlag == False then
                    (addFlag model cell.id, Cmd.none)
                else
                    if cell.isFlag == True then
                        (removeFlag model cell.id, Cmd.none)
                    else
                    (model, Cmd.none)
            else
                (model, Cmd.none)
    else
        (model, Cmd.none)

getCoordCell: Int ->(Int, Int )
getCoordCell id =
    (id // 10,  modBy 10 id)

getIdCell: (Int, Int) -> Int
getIdCell coord =
    if Tuple.first coord >= 0 && Tuple.first coord < 10 && Tuple.second coord >= 0 && Tuple.second coord < 10 then 
        (Tuple.first coord * 10) + Tuple.second coord
    else
        -1

getCellById : Model -> Int -> Cell
getCellById model id =
    let
        listCellId = List.filter (\cell -> cell.id == id) model.cells
    in
    case listCellId of
        [] -> { id = -1, isMine = False, nbBombAround = 0, revealed = True, isFlag = False }
        hd :: lt -> hd
        
onRightClick : (Msg, Bool) -> Html.Attribute Msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Decode.succeed msg)

updateMines: Model->List(Int,Int)->Model
updateMines model listMine =
    case listMine of
            [] -> model
            hd :: tl ->
                let 
                    newModel = isBomb model hd
                in
                    updateMines newModel tl

isBomb : Model -> (Int,Int) -> Model
isBomb model coordMine =
    { model
        |cells =
            model.cells
                |> List.map
                    (\cell ->
                        if Tuple.first coordMine ==  cell.id // 10 then
                            if Tuple.second coordMine == modBy 10 cell.id then
                                { cell | isMine = True }
                            else
                                cell
                        else
                            cell
                        ), 
                        nbBomb = model.nbBomb + 1
    }

updateNbBombAround : Model -> Model
updateNbBombAround model =
    { model
        |cells =
            model.cells
                |> List.map
                    (\cell ->
                            { cell | nbBombAround = numberBomb model cell }
                        )
    }

numberBomb: Model -> Cell -> Int
numberBomb model cell =
    let 
        coord = (getCoordCell cell.id)
        nbLeft = updateNumberBomb model (((-) (Tuple.first coord) 1), (Tuple.second coord))
        nbRight = updateNumberBomb model (((+) (Tuple.first coord) 1), (Tuple.second coord))
        nbUp = updateNumberBomb model ((Tuple.first coord), ((-)(Tuple.second coord) 1))
        nbDown = updateNumberBomb model ((Tuple.first coord), ((+)(Tuple.second coord) 1))
        nbLeftUp = updateNumberBomb model (((-) (Tuple.first coord) 1), ((-)(Tuple.second coord) 1))
        nbLeftDown = updateNumberBomb model (((-) (Tuple.first coord) 1), ((+)(Tuple.second coord) 1))
        nbRightUp = updateNumberBomb model (((+) (Tuple.first coord) 1), ((-)(Tuple.second coord) 1))
        nbRightDown = updateNumberBomb model (((+) (Tuple.first coord) 1), ((+)(Tuple.second coord) 1))
    in
        nbLeft + nbRight + nbUp + nbDown + nbLeftUp + nbLeftDown + nbRightUp + nbRightDown

updateNumberBomb: Model -> (Int, Int) -> Int
updateNumberBomb model coord =
    List.length (List.filter
        (\cell ->
            (Tuple.first coord) == (Tuple.first (getCoordCell cell.id)) 
                && (Tuple.second coord) == (Tuple.second (getCoordCell cell.id)) 
                && cell.isMine)
            model.cells)

addFlag : Model -> Int -> Model
addFlag model id = 
    { model
        | cells =
            model.cells
                |> List.map
                    (\cell ->
                        if id == cell.id then
                            { cell | isFlag = True }
                        else
                            cell
                    ), 
                    nbBomb = model.nbBomb - 1
    }

removeFlag : Model -> Int -> Model
removeFlag model id = 
    { model
        | cells =
            model.cells
                |> List.map
                    (\cell ->
                        if id == cell.id then
                            { cell | isFlag = False }
                        else
                            cell
                    ), nbBomb = model.nbBomb + 1
    }

revalAdjacentCell : Int -> Model -> Model
revalAdjacentCell id model =
        let
            cell = getCellById model id
            coord = (getCoordCell cell.id)
        in
        if cell.nbBombAround == 0 then
            revalCells (getIdCell ((Tuple.first coord), (Tuple.second coord))) model
                |> revalCells (getIdCell (((-) (Tuple.first coord) 1), (Tuple.second coord)))
                |> revalCells (getIdCell (((+) (Tuple.first coord) 1), (Tuple.second coord)))
                |> revalCells (getIdCell((Tuple.first coord), ((-)(Tuple.second coord) 1)))
                |> revalCells (getIdCell ((Tuple.first coord), ((+)(Tuple.second coord) 1)))
                |> revalCells (getIdCell (((-) (Tuple.first coord) 1), ((-)(Tuple.second coord) 1)))
                |> revalCells (getIdCell (((-) (Tuple.first coord) 1), ((+)(Tuple.second coord) 1)))
                |> revalCells (getIdCell (((+) (Tuple.first coord) 1), ((-)(Tuple.second coord) 1)))
                |> revalCells (getIdCell (((+) (Tuple.first coord) 1), ((+)(Tuple.second coord) 1)))
        else
            revalCells (getIdCell ((Tuple.first coord), (Tuple.second coord))) model

revalCells : Int -> Model -> Model
revalCells id model = 
    let 
        newModel = (
            if id >= 0 then
                { model
                    | cells =
                        model.cells
                            |> List.map
                                (\cell ->
                                    if id == cell.id then
                                        { cell | revealed = True }
                                    else
                                        cell
                                ), nbCellNotRevelead = 0
                }
            else
                {model | nbCellNotRevelead = 0})
    in
    allCellNotRevealed newModel newModel.cells

revalMine : Model -> Model
revalMine model = 
    { model
        | cells =
            model.cells
                |> List.map
                    (\cell ->
                        if cell.isMine == True then
                            { cell | revealed = True }
                        else
                            cell
                    ), gameOver = True
    }


allCellNotRevealed: Model->List(Cell)->Model
allCellNotRevealed model listCell =
    case listCell of
            [] -> model
            hd :: tl ->
                let 
                    newModel = updateNbCellNotRevalead model hd
                in
                    allCellNotRevealed newModel tl

updateNbCellNotRevalead : Model -> Cell -> Model
updateNbCellNotRevalead model cell =
    if cell.isMine == False && cell.revealed == False then
        { model| nbCellNotRevelead = model.nbCellNotRevelead + 1}
    else
        model


viewCell : Model -> Cell -> Html Msg
viewCell model cell =
    button
        [ onClick (RevealCell cell), onRightClick ((AddFlag cell), True), style "width" "50px", style "height" "50px"]
        [ if cell.revealed then
            if cell.isMine then
                if model.gameOver == True && cell.isFlag == True then
                    text "🚩💣 "
                else
                    text "💣 "
            else
                text (String.fromInt (cell.nbBombAround))
          else
            if cell.isFlag == True then
                text "🚩 "
            else
                text ""
        ]

viewGrille : Model -> Html Msg
viewGrille model =
    div [ style "display" "grid", style "grid-template-columns" "repeat(10, 50px)" ]
        (List.map (viewCell model)
            model.cells
        )

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Welcome to the minesweeper !" ]
        , text "click on a cell to start !"
        , br [] []
        , (
            if model.gameOver == True then
                text "Game Over"
            else if model.nbCellNotRevelead == 0 then
                text "win" 
            else
                text ""
        )
        , viewGrille model
        , text (String.fromInt (model.nbBomb))
        -- , br [] []
        -- , input [ placeholder "Enter Save code", onInput LoadSave ] []
        ]

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }