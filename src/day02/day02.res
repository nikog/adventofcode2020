open Belt

@bs.val external __dirname: string = "__dirname"

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input")->Js.String.split("\n", _)

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
  }
}

Js.log(readInput()->Part01.make)

module Part02 = {
  let re = %re("/(\d*).(\d*)\s(.):\s(.*)/")
  let make = input => {
    input
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
  }
}

Js.log(readInput()->Part02.make)
