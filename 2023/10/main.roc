app "202307part1"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task, await },
        List.{contains}
    ]
    provides [main] to pf

Direction : [North, West, South, East]
TileType : [NorthSouth, WestEast, NorthEast, NorthWest, SouthWest, SouthEast, NoPipe, Start]
Tile : {
    type: TileType,
    value: I32
}
Puzzle : {
    width : I32,
    height: I32,
    tiles : List Tile
}
Connections : {
    north: Result Coord [NotConnected],
    west:  Result Coord [NotConnected],
    south: Result Coord [NotConnected],
    east:  Result Coord [NotConnected],
}
Coord : {
    x: I32,
    y: I32
}

emptyPuzzle : Puzzle
emptyPuzzle = {
    width : 0, 
    height: 0,
    tiles : []
}

main =
    puzzle <- await (Task.loop emptyPuzzle readLine)

    dbg puzzle

    part1 = solvePart1 puzzle
    {} <- Stdout.line "Part 1: \(Num.toStr part1)" |> await

    part2 = solvePart2 puzzle
    {} <- Stdout.line "Part 2: \(Num.toStr part2)" |> await

    Stdout.line ""

getTile : Puzzle, Coord -> Tile
getTile = \puzzle, coord ->
    r = if coord.x < 0 || coord.x >= puzzle.width  then {type: NoPipe, value: 0}
        else if coord.y < 0 || coord.y >= puzzle.height then {type: NoPipe, value: 0}
        else puzzle.tiles |> List.get (Num.toNat (coord.y * puzzle.width + coord.x)) |> unwrap
    #dbg "Tile at \(Inspect.toStr coord) is \(Inspect.toStr r)"
    r

setTile : Puzzle, Coord, Tile -> Puzzle
setTile = \puzzle, coord, newTile ->
    if      coord.x < 0 || coord.x >= puzzle.width  then crash "setTile -> x out of range"
    else if coord.y < 0 || coord.y >= puzzle.height then crash "setTile -> y out of range"
    else 
        idx = Num.toNat (coord.y * puzzle.width + coord.x)
        { puzzle &
          tiles: List.update puzzle.tiles idx \_ -> newTile
        }
    
findConnectedTiles : Puzzle, Coord -> Connections
findConnectedTiles = \puzzle, coord ->
    north = {coord & y: coord.y - 1}
    west  = {coord & x: coord.x - 1}
    south = {coord & y: coord.y + 1}
    east  = {coord & x: coord.x + 1}

    dbg { north: north, west: west, south: south, east: east }

    {
        north: if [NorthSouth, SouthWest, SouthEast] |> contains (getTile puzzle north |> .type) then Ok north else Err NotConnected,
        west:  if [WestEast, NorthEast, SouthEast]   |> contains (getTile puzzle west  |> .type) then Ok west  else Err NotConnected,
        south: if [NorthSouth, NorthEast, NorthWest] |> contains (getTile puzzle south |> .type) then Ok south else Err NotConnected,
        east:  if [WestEast, NorthWest, SouthWest]   |> contains (getTile puzzle east  |> .type) then Ok east  else Err NotConnected,
    }

solvePart1 : Puzzle -> I32
solvePart1 = \puzzle ->
    startIx = puzzle.tiles |> List.findFirstIndex (\tile -> tile.type == Start) |> unwrap |> Num.toI32
    startY  = startIx // puzzle.width
    startX  = startIx - (startY * puzzle.width)

    dbg startX
    dbg startY

    # There will be exactly two tiles connected to the start tile
    connectedTiles : Connections
    connectedTiles = findConnectedTiles puzzle {x: startX, y: startY}

    dbg connectedTiles

    puzzle
    |> \p -> connectedTiles.north |> Result.map (\tile -> markPath p South tile 1) |> Result.withDefault p # Mark the path to the north
    |> \p -> connectedTiles.west  |> Result.map (\tile -> markPath p East  tile 1) |> Result.withDefault p # Mark the path to the west
    |> \p -> connectedTiles.south |> Result.map (\tile -> markPath p North tile 1) |> Result.withDefault p # Mark the path to the south
    |> \p -> connectedTiles.east  |> Result.map (\tile -> markPath p West  tile 1) |> Result.withDefault p # Mark the path to the east
    |> .tiles           # Get the tiles list
    |> List.dropIf \tile -> tile.type == Start # Remove the start tile since its count is bad
    |> List.map .value  # Get the value of each tile
    |> List.max         # Return the highest value
    |> Result.onErr \_ -> crash "No max"
    |> Result.withDefault 0

# Mark the path arriving at "coord" from "direction"
# "markPath puzzle South {x, y}" means, mark the path
# starting at {x, y}, entering from the South.
markPath : Puzzle, Direction, Coord, I32 -> Puzzle
markPath = \puzzle, comingFrom, coord, pathLength ->
    dbg "Marking \(Inspect.toStr coord) coming from \(Inspect.toStr comingFrom); Path length = \(Num.toStr pathLength)"
    
    currentTile = getTile puzzle coord
    if currentTile.value != 0 && currentTile.value <= pathLength 
    then
        # Only continue if the currently stored length
        # at this tile is greater than the current path
        # length. This ensures we only mark tiles that
        # we can reach quicker via this path than via
        # a previously explored path
        puzzle
    else
        newDirection : Result Direction [Finished]
        newDirection = when currentTile.type is
            NorthSouth if comingFrom == South -> Ok North
            NorthSouth if comingFrom == North -> Ok South
            WestEast   if comingFrom == West  -> Ok East
            WestEast   if comingFrom == East  -> Ok West
            NorthEast  if comingFrom == North -> Ok East
            NorthEast  if comingFrom == East  -> Ok North
            NorthWest  if comingFrom == North -> Ok West
            NorthWest  if comingFrom == West  -> Ok North
            SouthWest  if comingFrom == South -> Ok West
            SouthWest  if comingFrom == West  -> Ok South
            SouthEast  if comingFrom == South -> Ok East
            SouthEast  if comingFrom == East  -> Ok South
            Start -> Err Finished # Done marking
            _ -> crash "markPath invalid tile for nextDirection"

        newPuzzle = puzzle
            |> setTile coord {currentTile & value: pathLength}
        
        when newDirection is
            Ok North -> markPath newPuzzle South {coord & y: coord.y - 1} (pathLength + 1)
            Ok West  -> markPath newPuzzle East  {coord & x: coord.x - 1} (pathLength + 1)
            Ok South -> markPath newPuzzle North {coord & y: coord.y + 1} (pathLength + 1)
            Ok East  -> markPath newPuzzle West  {coord & x: coord.x + 1} (pathLength + 1)
            Err Finished -> newPuzzle

solvePart2 : Puzzle -> I64
solvePart2 = \_ -> 42
   
readLine = \puzzle ->
    parseLine : Str -> List Tile
    parseLine = \str ->
        str
        |> Str.graphemes
        |> List.map \g -> when g is
            "|" -> { type: NorthSouth, value: 0}
            "-" -> { type: WestEast, value: 0}
            "L" -> { type: NorthEast, value: 0}
            "J" -> { type: NorthWest, value: 0}
            "7" -> { type: SouthWest, value: 0}
            "F" -> { type: SouthEast, value: 0}
            "." -> { type: NoPipe, value: 0}
            "S" -> { type: Start, value: 0}
            _ -> crash "Invalid input"

    result <- await (Stdin.line)
    state =
        when result is
            Input line -> 
                tiles = parseLine line
                newPuzzle = {
                    width  : List.len tiles    |> Num.toI32, 
                    height : puzzle.height + 1 |> Num.toI32, 
                    tiles  : List.concat puzzle.tiles tiles
                }
                Step newPuzzle
            End -> Done puzzle
    Task.ok state

unwrap = \result ->
    when result is 
        Ok a -> a
        Err _ -> crash "unwrap failed"
