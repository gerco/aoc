app "202303"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ await },
    ]
    provides [main] to pf

period = "." |> Str.toUtf8 |> List.get 0 |> unwrap
zero = "0" |> Str.toUtf8 |> List.get 0 |> unwrap
nine = "9" |> Str.toUtf8 |> List.get 0 |> unwrap
asterisk = "*" |> Str.toUtf8 |> List.get 0 |> unwrap

Engine : List List U8
EnginePart : { x : List Nat, y : Nat, n : U32 }
Symbol : { x : Nat, y : Nat, v : U8 }

main =
    engine <- await (Task.loop [] readLine)

    # Find all the symbols in the engine definition
    symbols : List Symbol
    symbols =
        engine
        |> List.mapWithIndex findSymbolsInStr
        |> List.joinMap \lst -> lst
    # dbg symbols

    # Find all digits touching a symbol in any of the 8 directions
    digits : List Symbol
    digits =
        symbols
        |> List.joinMap \sym -> findTouchingDigits engine sym
    # dbg digits

    # Expand all the digits found into their full number (4 -> 124)
    engineParts : Set EnginePart
    engineParts =
        digits
        |> List.map \digit -> findEngineParts engine digit
        |> Set.fromList

    # Sum up all the parts
    totalPartNumbers =
        engineParts
        |> Set.toList
        |> List.map .n
        |> List.walk 0 Num.add

    # Find all the gears (* symbols that are touching exactly two engine parts)
    gears =
        symbols # Find all potential gears
        |> List.keepIf \sym -> sym.v == asterisk # Now add the list of parts they are touching
        |> List.map \ast -> {
            gear: ast,
            parts: engineParts
            |> Set.toList
            |> List.keepIf \part -> isPartTouching part ast,
        } # Keep only those touching exactly two parts
        |> List.keepIf \gear -> List.len gear.parts == 2

    totalGearRatio =
        gears # Calculate the ratio of each gear
        |> List.map \gear -> List.map gear.parts .n
        |> List.map \parts -> List.walk parts 1 Num.mul # Total the ratios
        |> List.walk 0 Num.add

    # Print the result
    Stdout.line "Total of part numbers: \(Num.toStr totalPartNumbers)\nTotal gear ratio: \(Num.toStr totalGearRatio)"

isPartTouching : EnginePart, Symbol -> Bool
isPartTouching = \part, sym ->
    ((sym.y - 1) == part.y && List.contains part.x (sym.x - 1))
    || ((sym.y - 1) == part.y && List.contains part.x (sym.x))
    || ((sym.y - 1) == part.y && List.contains part.x (sym.x + 1))
    || ((sym.y) == part.y && List.contains part.x (sym.x - 1))
    || ((sym.y) == part.y && List.contains part.x (sym.x + 1))
    || ((sym.y + 1) == part.y && List.contains part.x (sym.x - 1))
    || ((sym.y + 1) == part.y && List.contains part.x (sym.x))
    ||
    ((sym.y + 1) == part.y && List.contains part.x (sym.x + 1))

findEngineParts : Engine, Symbol -> EnginePart
findEngineParts = \engine, digit ->
    row = List.get engine digit.y |> unwrap
    dl = digitsToTheLeftInclusive row digit.x
    dr = digitsToTheRightExclusive row digit.x

    partno =
        (List.concat dl dr)
        |> Str.fromUtf8
        |> unwrap
        |> Str.toU32
        |> unwrap

    {
        x: range (digit.x - (List.len dl - 1)) (digit.x + List.len dr),
        y: digit.y,
        n: partno,
    }

digitsToTheLeftInclusive : List U8, Nat -> List U8
digitsToTheLeftInclusive = \lst, pos ->
    chr = List.get lst pos |> unwrap
    if !(isDigit chr) then
        []
    else if pos == 0 then
        [chr]
    else
        List.concat (digitsToTheLeftInclusive lst (pos - 1)) [chr]

digitsToTheRightExclusive : List U8, Nat -> List U8
digitsToTheRightExclusive = \lst, pos ->
    if (pos + 1) >= List.len lst then
        []
    else
        chr = List.get lst (pos + 1) |> unwrap
        if isDigit chr then
            List.concat [chr] (digitsToTheRightExclusive lst (pos + 1))
        else
            []

findTouchingDigits : Engine, Symbol -> List Symbol
findTouchingDigits = \engine, sym ->
    locations =
        [
            { x: sym.x - 1, y: sym.y - 1 },
            { x: sym.x, y: sym.y - 1 },
            { x: sym.x + 1, y: sym.y - 1 },
            { x: sym.x - 1, y: sym.y },
            { x: sym.x + 1, y: sym.y },
            { x: sym.x - 1, y: sym.y + 1 },
            { x: sym.x, y: sym.y + 1 },
            { x: sym.x + 1, y: sym.y + 1 },
        ]
        |> List.keepIf \loc -> loc.y >= 0 && loc.y < List.len engine
        |> List.keepIf \loc -> loc.x >= 0 && loc.x < (List.get engine loc.y |> unwrap |> List.len)

    locations
    |> List.map \loc ->
        row = List.get engine loc.y |> unwrap
        val = List.get row loc.x |> unwrap
        { x: loc.x, y: loc.y, v: val }
    |> List.keepIf \loc -> isDigit loc.v

findSymbolsInStr = \str, row -> str
    |> List.mapWithIndex \chr, col -> { x: col, y: row, v: chr }
    |> List.keepIf isSymbol

isSymbol : Symbol -> Bool
isSymbol = \sym -> sym.v != period && !(isDigit sym.v)

isDigit : U8 -> Bool
isDigit = \v -> v >= zero && v <= nine

readLine = \engine ->
    result <- await Stdin.line
    state =
        when result is
            Input line -> Step (List.append engine (line |> Str.toUtf8))
            End -> Done engine
    Task.ok state

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
