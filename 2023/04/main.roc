app "202304"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ await },
        parser.Core.{ Parser, oneOf, const, keep, skip, sepBy, many },
        parser.String.{ string, digits, parseStr, codeunit },
    ]
    provides [main] to pf

Card : {
    number : Nat,
    winning : List Nat,
    guesses : List Nat,
}

main =
    cards <- await (Task.loop [] readLine)

    totalScore =
        cards
        |> List.map cardScore
        |> List.sum

    totalCards =
        cards
        |> List.map \card -> winCards cards card
        |> List.sum

    Stdout.line "Score: \(Num.toStr totalScore)\nTotal cards: \(Num.toStr totalCards)"

cardScore : Card -> Nat
cardScore = \card ->
    when countMatchingNumbers card is
        0 -> 0
        n -> Num.powInt 2 (n - 1)

winCards : List Card, Card -> Nat
winCards = \cards, card ->
    when countMatchingNumbers card is
        0 -> 1
        n ->
            range 0 (n - 1)
            |> List.map (\i -> List.get cards (card.number + i) |> unwrap)
            |> List.map \wonCard -> winCards cards wonCard
            |> List.sum
            |> Num.add 1

# winCards : List Card, Card -> List Card
# winCards = \cards, card ->
#     when countMatchingNumbers card is
#         0 -> [card]
#         n ->
#             range 0 (n-1)
#             |> List.map (\i -> List.get cards (card.number + i) |> unwrap)
#             |> List.joinMap \wonCard -> winCards cards wonCard
#             |> List.prepend card

countMatchingNumbers : Card -> Nat
countMatchingNumbers = \card ->
    card.guesses
    |> List.keepIf \guess -> List.contains card.winning guess
    |> List.len

expect cardScore { guesses: [83, 86, 6, 31, 17, 9, 48, 53], number: 1, winning: [41, 48, 83, 86, 17] } == 8
expect cardScore { guesses: [61, 30, 68, 82, 17, 32, 24, 19], number: 2, winning: [13, 32, 20, 16, 61] } == 2

readLine = \cards ->
    result <- await Stdin.line
    state =
        when result is
            Input line -> Step (List.append cards (line |> parseLine))
            End -> Done cards
    Task.ok state

parseLine : Str -> Card
parseLine = \line ->
    result = parseStr parseCard line
    # dbg result
    when result is
        Ok card -> card
        Err e ->
            dbg
                e

            crash "Unable to parse card"

parseCard : Parser (List U8) Card
parseCard =
    const
        (\n -> \numbers -> {
                number: n,
                winning: List.first numbers |> unwrap,
                guesses: List.last numbers |> unwrap,
            })
    |> skip (string "Card")
    |> skip whitespace
    |> keep digits
    |> skip (codeunit ':')
    |> skip whitespace
    |> keep (sepBy parseListOfNumbers (codeunit '|'))

expect
    parseStr parseCard "Card 1: 12 34 56 78 90 | 23 34 45 56 78 89 90 12"
    == Ok {
        number: 1,
        winning: [12, 34, 56, 78, 90],
        guesses: [23, 34, 45, 56, 78, 89, 90, 12],
    }

expect
    parseStr parseCard "Card  20: 90 42 20 45 98 86  1 13  9 25 |  8 93 14 29 23 59 61 62 85 15 72 89 21 91 92 66  4 90 31 10  5 87 79 47 11"
    == Ok {
        number: 20,
        winning: [90, 42, 20, 45, 98, 86, 1, 13, 9, 25],
        guesses: [8, 93, 14, 29, 23, 59, 61, 62, 85, 15, 72, 89, 21, 91, 92, 66, 4, 90, 31, 10, 5, 87, 79, 47, 11],
    }

parseListOfNumbers : Parser (List U8) (List Nat)
parseListOfNumbers =
    const \r -> r
    |> skip whitespace
    |> keep (sepBy digits whitespace)
    |> skip whitespace

expect parseStr parseListOfNumbers "1 2 3 4 5" == Ok [1, 2, 3, 4, 5]
expect parseStr parseListOfNumbers " 1 2 3 4 5" == Ok [1, 2, 3, 4, 5]
expect parseStr parseListOfNumbers " 1 2 3 4 5 " == Ok [1, 2, 3, 4, 5]

whitespace : Parser (List U8) (a -> a)
whitespace =
    const \w -> w
    |> skip (many (oneOf [codeunit ' ', codeunit '\t']))

unwrap = \result ->
    when result is
        Ok a -> a
        Err e ->
            dbg
                e

            crash "kaboom"

range : Nat, Nat -> List Nat
range = \from, until ->
    if from == until then
        [from]
    else
        List.concat [from] (range (from + 1) until)
