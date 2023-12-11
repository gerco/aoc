app "202307part1"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task, await },
    ]
    provides [main] to pf

Puzzle : List (List I64)

main =
    puzzle <- await (Task.loop [] readLine)

    dbg puzzle

    part1 = solvePart1 puzzle
    {} <- Stdout.line "Part 1: \(Num.toStr part1)" |> await

    part2 = solvePart2 puzzle
    {} <- Stdout.line "Part 2: \(Num.toStr part2)" |> await

    Stdout.line ""

solvePart1 : Puzzle -> I64
solvePart1 = \puzzle ->
    puzzle
    |> List.map nextValueInSeq
    |> List.sum

solvePart2 : Puzzle -> I64
solvePart2 = \puzzle ->
    puzzle
    |> List.map prevValueInSeq
    |> List.sum
   
nextValueInSeq : List I64 -> I64
nextValueInSeq = \seq ->
    if List.sum seq == 0
    then 
        0
    else
        a = seq |> List.dropLast 1
        b = seq |> List.dropFirst 1
        diff = List.map2 b a Num.sub

        increment = nextValueInSeq diff
        lastElement = List.last seq |> unwrap

        lastElement + increment

expect nextValueInSeq [0, 0, 0, 0] == 0
expect nextValueInSeq [3, 3, 3, 3, 3] == 3
            
prevValueInSeq : List I64 -> I64
prevValueInSeq = \seq ->
    if List.sum seq == 0
    then 
        0
    else
        a = seq |> List.dropLast 1
        b = seq |> List.dropFirst 1
        diff = List.map2 a b Num.sub

        decrement = prevValueInSeq diff
        firstElement = List.first seq |> unwrap

        dbg "\(Inspect.toStr seq) -> \(Num.toStr firstElement) - \(Num.toStr decrement)"
        firstElement + decrement
            
expect prevValueInSeq [0, 0, 0, 0] == 0
expect prevValueInSeq [3, 3, 3, 3, 3] == 3
expect prevValueInSeq [10, 13, 16, 21, 30, 45] == 5

readLine = \puzzle ->
    parseLine : Str -> List I64
    parseLine = \str ->
        dbg "Parsing \(str)"
        str
        |> Str.split " "
        |> List.map Str.toI64
        |> List.map \r -> Result.withDefault r 0

    result <- await (Stdin.line)
    state =
        when result is
            Input line -> Step (puzzle |> List.append (parseLine line))
            End -> Done puzzle
    Task.ok state

unwrap = \result ->
    when result is 
        Ok a -> a
        Err _ -> crash "unwrap failed"
