app "202307part2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task, await },
        parser.Core.{ Parser, oneOf, const, keep, skip, many },
        parser.String.{ string, digits, parseStr, codeunit },
        List.{ contains },
    ]
    provides [main] to pf

Card : Str
HandType : [Unknown, FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard]
Hand : {
    cards : List Card,
    bid : Nat,
    type : HandType,
}
Puzzle : List Hand

main =
    puzzle <- await (Task.loop [] readLine)

    winnings = solve puzzle

    Stdout.line "Part 2 winnings: \(Num.toStr winnings)"

solve : Puzzle -> Nat
solve = \hands ->
    hands
    |> List.map (\hand -> { hand & type: handType hand.cards })
    |> List.sortWith handComparator
    |> List.walkWithIndex 0 \sum, hand, idx -> sum + (hand.bid * (idx + 1))

handComparator : Hand, Hand -> [LT, EQ, GT]
handComparator = \a, b ->
    when handTypeComparator a.type b.type is
        EQ -> cardsComparator a.cards b.cards
        other -> other

handTypeComparator : HandType, HandType -> [LT, EQ, GT]
handTypeComparator = \a, b ->
    aType = handTypeValue a
    bType = handTypeValue b

    if aType < bType then
        LT
    else if aType > bType then
        GT
    else
        EQ

cardsComparator : List Card, List Card -> [LT, EQ, GT]
cardsComparator = \a, b ->
    if a == [] || b == [] then
        EQ
    else
        aCard = a |> List.get 0 |> Result.map cardValue |> unwrap
        bCard = b |> List.get 0 |> Result.map cardValue |> unwrap

        if aCard < bCard then
            LT
        else if aCard > bCard then
            GT
        else
            cardsComparator (List.dropFirst a 1) (List.dropFirst b 1)

expect cardsComparator ("KK677" |> Str.graphemes) ("KTJJT" |> Str.graphemes) == GT

handType : List Card -> HandType
handType = \cards ->
    incrementCardCount : [Present Nat, Missing] -> [Present Nat, Missing]
    incrementCardCount = \v ->
        when v is
            Present n -> Present (n + 1)
            Missing -> Present 1

    cardCounts : List Nat
    cardCounts =
        List.walk
            cards
            (Dict.empty {})
            (\counts, card -> Dict.update counts card incrementCardCount)
        |> Dict.values

    basicType : HandType
    basicType =
        if cardCounts |> contains 5 then
            FiveOfAKind
        else if cardCounts |> contains 4 then
            FourOfAKind
        else if cardCounts |> contains 3 && cardCounts |> contains 2 then
            FullHouse
        else if cardCounts |> contains 3 then
            ThreeOfAKind
        else if cardCounts |> List.keepIf (\n -> n == 2) |> List.len == 2 then
            TwoPair
        else if cardCounts |> contains 2 then
            OnePair
        else
            HighCard

    if !(cards |> contains "J") then
        basicType
    else
        cards
        |> List.walkWithIndex basicType (\bestType, cardToReplace, idx ->
            candidateType = if cardToReplace != "J" then
                    basicType
                else
                    tryAllReplacementCards cards idx
            when handTypeComparator bestType candidateType is
                LT -> candidateType
                _ -> bestType)

replaceCard : List Card, Nat, Card -> List Card
replaceCard = \list, idx, replacement ->
    List.update list idx \_ -> replacement

expect replaceCard ("23456" |> Str.graphemes) 0 "J" |> Str.joinWith "" == "J3456"
expect replaceCard ("23456" |> Str.graphemes) 1 "J" |> Str.joinWith "" == "2J456"
expect replaceCard ("23456" |> Str.graphemes) 2 "J" |> Str.joinWith "" == "23J56"
expect replaceCard ("23456" |> Str.graphemes) 3 "J" |> Str.joinWith "" == "234J6"
expect replaceCard ("23456" |> Str.graphemes) 4 "J" |> Str.joinWith "" == "2345J"

tryAllReplacementCards : List Card, Nat -> HandType
tryAllReplacementCards = \cards, idx ->
    cardsWithoutJ = "AKQT98765432" |> Str.graphemes

    cardsWithoutJ
    |> List.walk Unknown \bestType, replacementCard ->
        replacementCards = replaceCard cards idx replacementCard
        candidateType = handType replacementCards
        dbg "\(Str.joinWith replacementCards "") -> \(Inspect.toStr candidateType)"
        when handTypeComparator bestType candidateType is
            LT -> candidateType
            _ -> bestType

expect tryAllReplacementCards ("22222" |> Str.graphemes) 0 == FiveOfAKind
expect tryAllReplacementCards ("J2222" |> Str.graphemes) 0 == FiveOfAKind
expect tryAllReplacementCards ("22J33" |> Str.graphemes) 2 == FullHouse

handTypeValue : HandType -> Nat
handTypeValue = \type ->
    when type is
        Unknown -> 0
        FiveOfAKind -> 7
        FourOfAKind -> 6
        FullHouse -> 5
        ThreeOfAKind -> 4
        TwoPair -> 3
        OnePair -> 2
        HighCard -> 1

cardValue : Card -> U8
cardValue = \card ->
    when card is
        "A" -> 14
        "K" -> 13
        "Q" -> 12
        "T" -> 10
        "J" -> 1
        _ -> Str.toU8 card |> unwrap

handParser : Parser (List U8) Hand
handParser =
    const \cards -> \bid -> { cards: cards, bid: bid, type: Unknown }
    |> keep
        (
            many
                (
                    oneOf [
                        string "A",
                        string "K",
                        string "Q",
                        string "T",
                        string "9",
                        string "8",
                        string "7",
                        string "6",
                        string "5",
                        string "4",
                        string "3",
                        string "2",
                        string "J",
                    ]
                )
        )
    |> skip whitespace
    |> keep digits

readLine = \hands ->
    parseHand : Str -> Hand
    parseHand = \line ->
        when parseStr handParser line is
            Ok hand -> hand
            Err e ->
                crash "Unable to parse hand"

    result <- await (Stdin.line)
    state =
        when result is
            Input line -> Step (List.append hands (parseHand line))
            End -> Done hands
    Task.ok state

whitespace : Parser (List U8) _
whitespace =
    const \a -> a
    |> skip (many (oneOf [codeunit ' ', codeunit '\t', codeunit '\n']))

unwrap = \result ->
    when result is
        Ok a -> a
        Err _ -> crash "unwrap failed"
