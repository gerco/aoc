app "202301"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Stdin, pf.Task.{ await }]
    provides [main] to pf

main =
    total <- Task.loop 0 readLine |> await
    Stdout.line "Calibration value: \(Num.toStr total)"

readLine = \total ->
    result <- await Stdin.line
    state =
        when result is
            Input line -> Step (total + calibrationValue line)
            End -> Done total
    Task.ok state

calibrationValue : Str -> U32
calibrationValue = \line ->
    result = Str.walkScalars line { first: None, last: None } findFirstLastDigit
    first =
        when result.first is
            None -> ""
            Value v -> Num.toStr v
    last =
        when result.last is
            None -> ""
            Value v -> Num.toStr v
    val = "\(first)\(last)"
    dbg
        "\(line) -> \(val)"

    when Str.toU32 val is
        Ok v -> v
        Err InvalidNumStr -> 0

findFirstLastDigit : State, U32 -> State
findFirstLastDigit = \state, scalar ->
    if scalar >= 48 && scalar <= 57 && state.first == None then
        { first: Value (scalar - 48), last: Value (scalar - 48) }
    else if scalar >= 48 && scalar <= 57 && state.first != None then
        { state & last: Value (scalar - 48) }
    else
        state

State : {
    first : Maybe U32,
    last : Maybe U32,
}

Maybe t : [Value t, None]
