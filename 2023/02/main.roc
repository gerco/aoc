app "202301"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout, pf.Stdin, pf.Task.{ await },
        parser.Core.{ Parser, map, oneOf, const, keep, skip, sepBy },
        parser.String.{ string, digits, parseStr },
    ]
    provides [main] to pf

maxRed   = 12
maxGreen = 13
maxBlue  = 14

Solution : {
    sumOfIds: Nat,
    power: Nat
}
emptySolution : Solution
emptySolution = { sumOfIds: 0, power: 0 }

main =
    total <- await (Task.loop emptySolution readLine)
    _ <- await (Stdout.line "Sum of possible game IDs: \(Num.toStr total.sumOfIds)")
    _ <- await (Stdout.line "Sum of game powers: \(Num.toStr total.power)")
    Stdout.line ""

readLine = \total ->
    result <- await Stdin.line
    state = when result is
        Input line -> Step (line |> parseLine |> evaluateGame total)
        End -> Done total
    Task.ok state

evaluateGame : Game, Solution -> Solution
evaluateGame = \game, total -> {
        sumOfIds: total.sumOfIds + (idIfGamePossible game),
        power: total.power + (powerForGame game)
    }

Color : [Red, Green, Blue]
Game : {
    id: Nat,
    grabs: List (List { col: Color, num: Nat })
}

parseColor : Parser (List U8) Color
parseColor = oneOf [
    string "red"   |> map (\_ -> Red),
    string "green" |> map (\_ -> Green),
    string "blue"  |> map (\_ -> Blue)
]

expect parseStr parseColor "red"   == Ok Red
expect parseStr parseColor "green" == Ok Green
expect parseStr parseColor "blue"  == Ok Blue

parseCubes = 
    const \num -> \col -> {num: num, col: col}
    |> keep digits
    |> skip (string " ")
    |> keep parseColor

expect parseStr parseCubes "3 red" == Ok {num: 3, col: Red}
expect parseStr parseCubes "5 green" == Ok {num: 5, col: Green}
expect parseStr parseCubes "12 blue" == Ok {num: 12, col: Blue}

parseGrab =
    sepBy parseCubes (string ", ")

expect parseStr parseGrab "3 red, 4 blue, 6 green" == Ok [
    {col: Red, num: 3}, {col: Blue, num: 4}, {col: Green, num: 6}
]

#parseGame : Parser (List U8) Game
#parseGame = string "Game " |> map (\_ -> newGame 42)
parseGame = 
    const (\id -> \grabs -> {id: id, grabs: grabs})
    |> skip (string "Game ")
    |> keep digits
    |> skip (string ": ")
    |> keep (sepBy parseGrab (string "; "))

expect
    result = parseStr parseGame "Game 1: 1 red, 2 green, 4 blue"
    dbg result
    result == Ok {id: 1, grabs: [[{col: Red, num: 1}, {col: Green, num: 2}, {col: Blue, num: 4}]]}

parseLine : Str -> Game
parseLine = \line -> 
    result = parseStr parseGame line
    #dbg result
    when result is
        Ok game -> game
        Err _ -> crash "Unable to parse game"

idIfGamePossible : Game -> Nat
idIfGamePossible = \game ->
    if List.all game.grabs isGrabPossible then game.id
    else 0

isGrabPossible : (List { col: Color, num: Nat }) -> Bool
isGrabPossible = \grab ->
    List.all grab isCubesPossible

isCubesPossible = \{col, num} ->
    when col is
        Red -> num   <= maxRed
        Green -> num <= maxGreen
        Blue -> num  <= maxBlue

powerForGame = \game ->
    s0 = {red: 0, green: 0, blue: 0}

    completedState = 
        List.walk game.grabs s0 \s1, grab ->
            List.walk grab s1 \s2, cubes ->
                when cubes.col is
                    Red   if s2.red   < cubes.num -> {s2 & red:   cubes.num}
                    Green if s2.green < cubes.num -> {s2 & green: cubes.num}
                    Blue  if s2.blue  < cubes.num -> {s2 & blue:  cubes.num}
                    _ -> s2

    completedState.red * completedState.green * completedState.blue
