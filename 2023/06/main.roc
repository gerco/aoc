app "202306"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task, await },
    ]
    provides [main] to pf

Race : { duration : Nat, distance : Nat }
newRace : Nat, Nat -> Race
newRace = \duration, distance -> {
    duration: duration,
    distance: distance,
}

# Time:        48     98     90     83
# Distance:   390   1103   1112   1360
part1Races : List Race
part1Races = [
    newRace 48 390,
    newRace 98 1103,
    newRace 90 1112,
    newRace 83 1360,
]

part2Races : List Race
part2Races = [
    newRace 48989083 390110311121360,
]

main =
    part1 = part1Races |> List.map numberOfWaysToWin |> List.product
    {} <- Stdout.line "Part 1: \(Num.toStr part1)" |> await

    part2 = part2Races |> List.map numberOfWaysToWin |> List.product
    {} <- Stdout.line "Part 2: \(Num.toStr part2)" |> await

    Stdout.line ""

numberOfWaysToWin : Race -> Nat
numberOfWaysToWin = \race ->
    numberOfWaysToWinInt race {
        currentButtonDuration: 0,
        waysToWin: 0,
    }

numberOfWaysToWinInt : Race, { currentButtonDuration : Nat, waysToWin : Nat } -> Nat
numberOfWaysToWinInt = \race, { currentButtonDuration, waysToWin } ->
    if currentButtonDuration > race.duration then
        waysToWin
    else
        won = totalDistance currentButtonDuration race.duration > race.distance
        newWaysToWin = if won then waysToWin + 1 else waysToWin
        numberOfWaysToWinInt race {
            currentButtonDuration: currentButtonDuration + 1,
            waysToWin: newWaysToWin,
        }

totalDistance : Nat, Nat -> Nat
totalDistance = \buttonDuration, raceDuration ->
    (raceDuration - buttonDuration) * buttonDuration

expect totalDistance 0 7 == 0
expect totalDistance 1 7 == 6
expect totalDistance 2 7 == 10
expect totalDistance 3 7 == 12
expect totalDistance 4 7 == 12
expect totalDistance 5 7 == 10
expect totalDistance 6 7 == 6
expect totalDistance 7 7 == 0
