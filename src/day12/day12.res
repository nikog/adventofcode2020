open Belt

type point = Point(int, int)

let applyUnits = (Point(x, y), Point(dx, dy), units) => Point(x + dx * units, y + dy * units)

exception Error
module Part01 = {
  let make = input => {
    let (endpoint, _) = input->Js.String2.split("\n")->Array.reduce((Point(0, 0), Point(1, 0)), ((
      pos,
      direction,
    ), instruction) => {
      let action = instruction->Js.String2.charAt(0)
      let units = instruction->Js.String2.substringToEnd(~from=1)->Int.fromString
      let Point(dx, dy) = direction

      switch (action, units) {
      | (action, Some(units)) =>
        switch action {
        | "E" => (applyUnits(pos, Point(1, 0), units), direction)
        | "N" => (applyUnits(pos, Point(0, -1), units), direction)
        | "W" => (applyUnits(pos, Point(-1, 0), units), direction)
        | "S" => (applyUnits(pos, Point(0, 1), units), direction)
        | "F" => (applyUnits(pos, direction, units), direction)
        | "L" when units == 90 => (pos, Point(dy, dx * -1))
        | "L" when units == 180 => (pos, Point(dx * -1, dy * -1))
        | "L" when units == 270 => (pos, Point(dy * -1, dx))
        | "R" when units == 90 => (pos, Point(dy * -1, dx))
        | "R" when units == 180 => (pos, Point(dx * -1, dy * -1))
        | "R" when units == 270 => (pos, Point(dy, dx * -1))
        | _ => (pos, direction)
        }
      | _ => raise(Error)
      }
    })

    let Point(x, y) = endpoint

    (abs(x) + abs(y))->Int.toString
  }
}

module Part02 = {
  let make = input => {
    let (endpoint, _) = input->Js.String2.split("\n")->Array.reduce((Point(0, 0), Point(10, -1)), ((
      shipPos,
      wpPos,
    ), instruction) => {
      let action = instruction->Js.String2.charAt(0)
      let units = instruction->Js.String2.substringToEnd(~from=1)->Int.fromString
      let Point(dx, dy) = wpPos

      switch (action, units) {
      | (action, Some(units)) =>
        switch action {
        | "E" => (shipPos, applyUnits(wpPos, Point(1, 0), units))
        | "N" => (shipPos, applyUnits(wpPos, Point(0, -1), units))
        | "W" => (shipPos, applyUnits(wpPos, Point(-1, 0), units))
        | "S" => (shipPos, applyUnits(wpPos, Point(0, 1), units))

        | "L" when units == 90 => (shipPos, Point(dy, dx * -1))
        | "R" when units == 270 => (shipPos, Point(dy, dx * -1))

        | "R" when units == 90 => (shipPos, Point(dy * -1, dx))
        | "L" when units == 270 => (shipPos, Point(dy * -1, dx))

        | "L" when units == 180 => (shipPos, Point(dx * -1, dy * -1))
        | "R" when units == 180 => (shipPos, Point(dx * -1, dy * -1))

        | "F" => (applyUnits(shipPos, wpPos, units), wpPos)

        | _ => (shipPos, wpPos)
        }
      | _ => raise(Error)
      }
    })

    let Point(x, y) = endpoint

    (abs(x) + abs(y))->Int.toString
  }
}

Solution.make(module(Part01), "day12/input")
Solution.make(module(Part02), "day12/input")
