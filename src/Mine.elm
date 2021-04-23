module Mine exposing (Mine, Options, generateRandomMines)

import Random exposing (Generator)
import Set


type alias Mine =
    ( Int, Int )


type alias Options =
    { width : Int
    , height : Int
    , minMines : Int
    , maxMines : Int
    , initialX : Int
    , initialY : Int
    }


randomMinesGeneratorList : Options -> Int -> Generator (List Mine)
randomMinesGeneratorList { width, height } len =
    Random.list len <|
        Random.pair
            (Random.int 0 width)
            (Random.int 0 height)


relaunch : Options -> List Mine -> Generator (List Mine)
relaunch ({ minMines, initialY, initialX } as options) l =
    if List.length l >= minMines && not (List.member ( initialX, initialY ) l) then
        Random.constant l

    else
        randomMinesGenerator options


randomMinesGenerator : Options -> Generator (List Mine)
randomMinesGenerator ({ width, height, minMines, maxMines } as options) =
    Random.int minMines maxMines
        |> Random.andThen (randomMinesGeneratorList options)
        |> Random.map (Set.fromList >> Set.toList)
        |> Random.andThen (relaunch options)


generateRandomMines : Options -> (List Mine -> msg) -> Cmd msg
generateRandomMines options msg =
    Random.generate msg (randomMinesGenerator options)
