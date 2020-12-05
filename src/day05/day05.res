open Belt

type split =
  | Left // F | L
  | Right // B | R

let binarySearch = (partitions, max) => {
  partitions->Array.reduce((0, max), ((start, end), letter) => {
    let mid = Js.Math.floor(Float.fromInt(start + end) /. 2.)

    switch letter {
    | Left => (start, mid)
    | Right => (mid + 1, end)
    }
  })->(((x, _)) => x)
}

exception Invalid_Input

let strToBinary = letters => letters->Js.String2.split("")->Array.map(letter =>
    switch letter {
    | "R" => Right
    | "L" => Left
    | "B" => Right
    | "F" => Left
    | _ => raise(Invalid_Input)
    }
  )

let getRowIds = input => input->Js.String2.split("\n")->Array.map(row => {
    let str = row
    let row = str->Js.String2.substring(~from=0, ~to_=7)->strToBinary->binarySearch(127)
    let seat = str->Js.String2.substringToEnd(~from=7)->strToBinary->binarySearch(7)

    row * 8 + seat
  })

module Part01 = {
  let make = input =>
    input->getRowIds->Array.reduce(0, (max, id) => id > max ? id : max)->Int.toString
}

module Part02 = {
  let findSeat = Array.reduce(_, (0, None), ((prev, found), id) =>
    switch (prev, id > prev + 1) {
    | (0, _) => (id, None)
    | (_, true) => (id, Some(prev + 1))
    | (_, _) => (id, found)
    }
  )

  let make = input =>
    input
    ->getRowIds
    ->SortArray.Int.stableSort
    ->findSeat
    ->(((_, found)) => found)
    ->Option.map(x => Js.Int.toString(x))
    ->Option.getWithDefault("Not found")
}

Solution.make(module(Part01), "day05/input")
Solution.make(module(Part02), "day05/input")
