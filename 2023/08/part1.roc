app "202308part1"
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
            Stdout.line "Part 1: \(Inspect.toStr answer)"

        Err _ -> crash "Error parsing puzzle"

solve : Puzzle -> Nat
solve = \puzzle ->
    travel : List Instruction, Node, Nat -> Nat
    travel = \instr, currentNode, steps ->
        if currentNode.name == "ZZZ" then steps
        else when List.first instr is
            Ok L -> travel (List.dropFirst instr 1) (Dict.get puzzle.nodes currentNode.left |> unwrap) (steps + 1)
            Ok R -> travel (List.dropFirst instr 1) (Dict.get puzzle.nodes currentNode.right |> unwrap) (steps + 1)
            Err ListWasEmpty -> travel puzzle.instr currentNode steps

    travel puzzle.instr (Dict.get puzzle.nodes "AAA" |> unwrap) 0

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
