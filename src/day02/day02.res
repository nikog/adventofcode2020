open Belt

@bs.val external __dirname: string = "__dirname"

let parse = x => {
  switch x {
  | Some([_, min, max, char, password]) => Some((min, max, char, password))
  | _ => None
  }
}

module Part01 = {
  let re = %re("/(\d*).(\d*)\s(.):\s(.*)/")
  let make = input => {
    input
    ->Js.String.split("\n", _)
    ->Array.map(row => Js.String.match_(re, row)->parse)
    ->Array.keepMap(x => x)
    ->Array.map(result => {
      let (min, max, char, password) = result

      let result = Js.Re.fromStringWithFlags(char, ~flags="g")->Js.String.match_(password)
      let min = Int.fromString(min)
      let max = Int.fromString(max)

      switch (result, min, max) {
      | (Some(r), Some(min), Some(max)) => {
          let res = Array.length(r)

          res >= min && res <= max
        }
      | _ => false
      }
    })
    ->Array.keep(x => x == true)
    ->Array.length
    ->Int.toString
  }
}

module Part02 = {
  let re = %re("/(\d*).(\d*)\s(.):\s(.*)/")
  let make = input => {
    input
    ->Js.String.split("\n", _)
    ->Array.map(row => Js.String.match_(re, row)->parse)
    ->Array.keepMap(x => x)
    ->Array.map(result => {
      let (pos1, pos2, char, password) = result

      let pos1 = Int.fromString(pos1)
      let pos2 = Int.fromString(pos2)

      switch (pos1, pos2) {
      | (Some(pos1), Some(pos2)) => {
          let charAtPos1 = Js.String.charAt(pos1 - 1, password)
          let charAtPos2 = Js.String.charAt(pos2 - 1, password)

          switch (charAtPos1 == char, charAtPos2 == char) {
          | (true, false) => true
          | (false, true) => true
          | _ => false
          }
        }
      | _ => false
      }
    })
    ->Array.keep(x => x == true)
    ->Array.length
    ->Int.toString
  }
}

let input = __dirname ++ "/input"

Solution.make(module(Part01), input)
Solution.make(module(Part02), input)
