open Belt

type point = Point(int, int)

type pos = Floor | EmptySeat | OccupiedSeat

let adjacent = [
  Point(-1, -1),
  Point(0, -1),
  Point(1, -1),
  Point(-1, 0),
  // None,
  Point(1, 0),
  Point(-1, 1),
  Point(0, 1),
  Point(1, 1),
]

let adjacentOccupied = (checkFn, map, Point(x, y)) => {
  adjacent->Array.reduce(0, (sum, Point(dX, dY)) => {
    sum + checkFn(map, Point(x, y), Point(dX, dY))
  })
}

let rec loopUntilSeated = (checkFn, tolerance, map) => {
  let checkAdjacent = adjacentOccupied(checkFn)

  let newMap = map->Array.mapWithIndex((y, row) => {
    row->Array.mapWithIndex((x, pos) => {
      switch pos {
      | EmptySeat when checkAdjacent(map, Point(x, y)) == 0 => OccupiedSeat
      | OccupiedSeat when checkAdjacent(map, Point(x, y)) >= tolerance => EmptySeat
      | EmptySeat | OccupiedSeat | Floor => pos
      }
    })
  })

  let hasNoChanges = newMap->Array.eq(map, (aRow, bRow) => {
    aRow->Array.eq(bRow, (a, b) => a == b)
  })

  switch hasNoChanges {
  | true => newMap
  | false => loopUntilSeated(checkFn, tolerance, newMap)
  }
}

exception InvalidCharacter
let mapInput = input => {
  input->Js.String2.split("\n")->Array.map(row => {
    row->Js.String2.split("")->Array.map(char => {
      switch char {
      | "#" => OccupiedSeat
      | "L" => EmptySeat
      | "." => Floor
      | _ => raise(InvalidCharacter)
      }
    })
  })
}

let countOccupied = map => {
  map->Array.reduce(0, (sum, row) => {
    sum + row->Array.reduce(0, (sum, pos) => {
      sum + (pos == OccupiedSeat ? 1 : 0)
    })
  })
}

let pointSum = (Point(ax, ay), Point(bx, by)) => Point(ax + bx, ay + by)

module Part01 = {
  let checkOccupied = (map, current, delta) => {
    let Point(x, y) = pointSum(current, delta)
    let pos = map[y]->Option.flatMap(row => row[x])

    switch pos {
    | Some(pos) when pos == OccupiedSeat => 1
    | _ => 0
    }
  }

  let loopUntilSeated = loopUntilSeated(checkOccupied, 4)

  let make = input => {
    input->mapInput->loopUntilSeated->countOccupied->Int.toString
  }
}

module Part02 = {
  let rec canSeeOccupiedSeat = (map, current, delta) => {
    let Point(x, y) = pointSum(current, delta)
    let pos = map[y]->Option.flatMap(row => row[x])

    switch pos {
    | Some(pos) when pos == Floor => canSeeOccupiedSeat(map, Point(x, y), delta)
    | Some(pos) when pos == OccupiedSeat => 1
    | _ => 0
    }
  }

  let loopUntilSeated = loopUntilSeated(canSeeOccupiedSeat, 5)

  let make = input => {
    input->mapInput->loopUntilSeated->countOccupied->Int.toString
  }
}

Solution.make(module(Part01), "day11/input")
Solution.make(module(Part02), "day11/input")
