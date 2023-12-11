app "202308part2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task, await },
        parser.Core.{ Parser, map, oneOf, const, keep, skip, many },
        parser.String.{ string, parseStr, codeunit, anyCodeunit },
    ]
    provides [main] to pf

Instruction : [L, R]
Node : {
    name: Str,
    left: Str,
    right: Str,
}
Puzzle : {
    instr: List Instruction,
    nodes: Dict Str Node
}

main =
    result <- await (readPuzzle)
    dbg result

    when result is
        Ok puzzle ->
            answer = solve puzzle
            Stdout.line "Part 2: \(Inspect.toStr answer)"

        Err _ -> crash "Error parsing puzzle"

allEndIn : List Node, Str -> Bool
allEndIn = \nodes, chr ->
    nodes 
    |> List.map \node -> node.name
    |> List.all (\str -> str |> Str.graphemes |> List.last |> unwrap == chr)

goLeft : Puzzle, Node -> Node
goLeft = \puzzle, node -> Dict.get puzzle.nodes node.left |> unwrap

goRight : Puzzle, Node -> Node
goRight = \puzzle, node -> Dict.get puzzle.nodes node.right |> unwrap
    
travel : Puzzle, List Instruction, List Node, Nat -> Nat
travel = \puzzle, instr, currentNodes, steps ->
    if currentNodes |> allEndIn "Z" then steps
    else when List.first instr is
        Ok L -> travel puzzle (List.dropFirst instr 1) (List.map currentNodes \n -> goLeft puzzle n) (steps + 1)
        Ok R -> travel puzzle (List.dropFirst instr 1) (List.map currentNodes \n -> goRight puzzle n) (steps + 1)
        Err ListWasEmpty -> travel puzzle puzzle.instr currentNodes steps

solve : Puzzle -> Nat
solve = \puzzle ->
    # Due to a compiler bug, the above functions cannot exist in here. That would be better
    # since they all need access to "puzzle" and this way it has to be passed around everywhere

    startNodes = puzzle.nodes
        |> Dict.keys
        |> List.keepIf \name -> name |> Str.graphemes |> List.last |> unwrap == "A"
        |> List.map \name -> Dict.get puzzle.nodes name |> unwrap

    dbg startNodes

    travel puzzle puzzle.instr startNodes 0

readPuzzle =
    inputText <- await (Task.loop "" readLine)
    Task.ok (parseStr puzzleParser inputText)

puzzleParser : Parser (List U8) Puzzle
puzzleParser = 
    const \instr -> \nodes -> {instr: instr, nodes: nodes}
    |> keep instructionsParser
    |> skip whitespace
    |> keep (map (many nodeParser) \nodes -> List.walk nodes (Dict.empty {}) \dict, node -> Dict.insert dict node.name node)

nodeParser : Parser (List U8) Node
nodeParser = 
    const \name -> \left -> \right -> {name: name, left: left, right: right}
    |> keep nodeName
    |> skip (string " = (")
    |> keep nodeName
    |> skip (string ", ")
    |> keep nodeName
    |> skip (string ")")
    |> skip whitespace

nodeName : Parser (List U8) Str
nodeName =
    const \a -> \b -> \c -> Str.fromUtf8 [a, b, c] |> unwrap
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> keep anyCodeunit

instructionsParser : Parser (List U8) (List Instruction)
instructionsParser = 
    const \i -> i
    |> keep (many (map (oneOf [string "L", string "R"]) \lr -> when lr is 
        "L" -> L
        "R" -> R
        _ -> crash "Invalid instruction"))

readLine = \text ->
    result <- await (Stdin.line)
    state =
        when result is
            Input line -> Step (text |> Str.concat line |> Str.concat "\n")
            End -> Done text
    Task.ok state

whitespace : Parser (List U8) _
whitespace =
    const \a -> a
    |> skip (many (oneOf [codeunit ' ', codeunit '\t', codeunit '\n']))

unwrap = \result ->
    when result is
        Ok a -> a
        Err e ->
            dbg
                e

            crash "kaboom"
