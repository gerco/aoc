app "202305"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task, await },
        parser.Core.{ Parser, oneOf, const, keep, skip, sepBy, many },
        parser.String.{ string, digits, parseStr, codeunit },
    ]
    provides [main] to pf

Seeds : List Nat
AlmanacMap : {
    srcStart : Nat,
    dstStart : Nat,
    length : Nat,
}
Almanac : {
    seedToSoil : List AlmanacMap,
    soilToFertilizer : List AlmanacMap,
    fertilizerToWater : List AlmanacMap,
    waterToLight : List AlmanacMap,
    lightToTemperature : List AlmanacMap,
    temperatureToHumidity : List AlmanacMap,
    humidityToLocation : List AlmanacMap,
}
Puzzle : {
    seeds : Seeds,
    almanac : Almanac,
}

main =
    inputFile <- await (Task.loop "" readLine)
    result = parseStr inputParser inputFile
    when result is
        Ok puzzle ->
            dbg
                puzzle

            part1 = solvePart1 puzzle
            part2 <- solvePart2 puzzle |> await
            Stdout.line "Part 1: \(Num.toStr part1)\nPart 2: \(Num.toStr part2)\n"

        Err e ->
            Stdout.line "Parsing failed: \(Inspect.toStr e)"

solvePart1 : Puzzle -> Nat
solvePart1 = \puzzle ->
    puzzle.seeds
    |> List.map \n -> mapSeed puzzle n
    |> List.min
    |> unwrap

Part2State : {
    puzzle : Puzzle,
    ranges : List { start : Nat, length : Nat },
    results : List Nat,
}

# Slow, but it works.
solvePart2 = \puzzle ->
    ranges =
        puzzle.seeds
        |> List.chunksOf 2 # Split into {start, length} pairs
        |> List.map \list -> { start: List.get list 0 |> unwrap, length: List.get list 1 |> unwrap }

    initialState = {
        puzzle: puzzle,
        ranges: ranges,
        results: [],
    }

    results <- Task.loop initialState solvePart2Int |> await
    dbg
        results

    Task.ok (results |> List.min |> unwrap)

solvePart2Int = \state ->
    if List.len state.ranges == 0 then
        Task.ok
            (
                Done state.results
            )
    else
        srr <- searchRange (List.get state.ranges 0 |> unwrap) state.puzzle |> await
        Task.ok
            (
                Step
                    { state &
                        ranges: List.dropFirst state.ranges 1,
                        results: List.append state.results srr,
                    }
            )

searchRange : { start : Nat, length : Nat }, Puzzle -> Task Nat *
searchRange = \range, puzzle ->
    {} <- (Stdout.line "Searching seed range \(Num.toStr range.start) \(Num.toStr range.length)") |> await
    lowestLocationNumber range (Num.toNat Num.maxU64) puzzle |> Task.ok

lowestLocationNumber : { start : Nat, length : Nat }, Nat, Puzzle -> Nat
lowestLocationNumber = \{ start, length }, currentLowest, puzzle ->
    if length == 0 then
        crash "Length == 0"
    else
        seedLocation = mapSeed puzzle start
        newLowest = Num.min seedLocation currentLowest
        if length == 1 then
            newLowest
        else
            newRange = { start: start + 1, length: length - 1 }
            dbg
                "Seed \(Num.toStr start) mapped to location \(Num.toStr seedLocation). The current lowest is \(Num.toStr newLowest)"

            lowestLocationNumber newRange newLowest puzzle

mapSeed : Puzzle, Nat -> Nat
mapSeed = \puzzle, seed ->
    seed
    |> searchMap puzzle.almanac.seedToSoil
    |> searchMap puzzle.almanac.soilToFertilizer
    |> searchMap puzzle.almanac.fertilizerToWater
    |> searchMap puzzle.almanac.waterToLight
    |> searchMap puzzle.almanac.lightToTemperature
    |> searchMap puzzle.almanac.temperatureToHumidity
    |> searchMap puzzle.almanac.humidityToLocation

searchMap : Nat, List AlmanacMap -> Nat
searchMap = \input, maps ->
    maps
    |> List.findFirst \m -> input >= m.srcStart && input < (m.srcStart + m.length)
    |> Result.map \m -> input - m.srcStart + m.dstStart
    |> Result.withDefault input

expect searchMap 98 [{ srcStart: 98, dstStart: 50, length: 2 }] == 50
expect searchMap 99 [{ srcStart: 98, dstStart: 50, length: 2 }] == 51
expect searchMap 1 [{ srcStart: 98, dstStart: 50, length: 2 }] == 1

inputParser : Parser (List U8) Puzzle
inputParser =
    listOfSeeds =
        const \seeds -> seeds
        |> skip (string "seeds:")
        |> skip whitespace
        |> keep listOfNumbers

    almanacMap =
        const
            \dstStart -> \srcStart -> \length -> { srcStart: srcStart, dstStart: dstStart, length: length }
        |> keep digits
        |> skip whitespace
        |> keep digits
        |> skip whitespace
        |> keep digits
        |> skip whitespace

    almanac =
        const \s2s -> \s2f -> \f2w -> \w2l -> \l2t -> \t2h -> \h2l -> {
                                    seedToSoil: s2s,
                                    soilToFertilizer: s2f,
                                    fertilizerToWater: f2w,
                                    waterToLight: w2l,
                                    lightToTemperature: l2t,
                                    temperatureToHumidity: t2h,
                                    humidityToLocation: h2l,
                                }
        |> skip (string "seed-to-soil map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "soil-to-fertilizer map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "fertilizer-to-water map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "water-to-light map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "light-to-temperature map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "temperature-to-humidity map:")
        |> skip whitespace
        |> keep (many almanacMap)
        |> skip (string "humidity-to-location map:")
        |> skip whitespace
        |> keep (many almanacMap)

    const \se -> \al -> { seeds: se, almanac: al }
    |> keep listOfSeeds
    |> keep almanac

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

listOfNumbers : Parser (List U8) (List Nat)
listOfNumbers =
    const \r -> r
    |> skip whitespace
    |> keep (sepBy digits whitespace)
    |> skip whitespace

expect parseStr listOfNumbers "1 2 3 4 5" == Ok [1, 2, 3, 4, 5]
expect parseStr listOfNumbers " 1 2 3 4 5" == Ok [1, 2, 3, 4, 5]
expect parseStr listOfNumbers " 1 2 3 \t 4 5 " == Ok [1, 2, 3, 4, 5]
expect parseStr listOfNumbers "1 2\t3 4 5 " == Ok [1, 2, 3, 4, 5]

unwrap = \result ->
    when result is
        Ok a -> a
        Err e ->
            dbg
                e

            crash "kaboom"
