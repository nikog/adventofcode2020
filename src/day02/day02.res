open Belt

@bs.val external __dirname: string = "__dirname"

let readInput = () => Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input")->Js.String.split("\n", _)

module Part01 = {
  let re = %re("/(\d*).(\d*)\s(.):\s(.*)/")
  let make = input => {
    input
    ->Array.map(row => Js.Re.exec_(re, row))
    ->Array.keepMap(x => x)
    ->Array.map(result => Js.Re.captures(result))
    ->Array.map(result => {
      let [_, min, max, char, password] = result->Array.map(Js.Nullable.toOption)

      switch (min, max, char, password) {
      | (Some(min), Some(max), Some(char), Some(password)) => {
          let result = Js.Re.fromStringWithFlags(char, ~flags="g")->Js.String.match_(password)
          let min = Int.fromString(min)
          let max = Int.fromString(max)

          switch (result, min, max) {
          | (Some(r), Some(min), Some(max)) => {
              let res = Array.length(r)

              Some(res >= min && res <= max)
            }
          | _ => Some(false)
          }
        }
      | _ => Some(false)
      }
    })
    ->Array.keep(x => x == Some(true))
    ->Array.length
  }
}

Js.log(readInput()->Part01.make)

module Part02 = {
  let re = %re("/(\d*).(\d*)\s(.):\s(.*)/")
  let make = input => {
    input
    ->Array.map(row => Js.Re.exec_(re, row))
    ->Array.keepMap(x => x)
    ->Array.map(result => Js.Re.captures(result))
    ->Array.map(result => {
      let [_, pos1, pos2, char, password] = result->Array.map(Js.Nullable.toOption)

      switch (pos1, pos2, char, password) {
      | (Some(pos1), Some(pos2), Some(char), Some(password)) => {
          let pos1 = Int.fromString(pos1)
          let pos2 = Int.fromString(pos2)

          switch (pos1, pos2) {
          | (Some(pos1), Some(pos2)) => {
              let charAtPos1 = Js.String.charAt(pos1 - 1, password)
              let charAtPos2 = Js.String.charAt(pos2 - 1, password)

              switch (charAtPos1 == char, charAtPos2 == char) {
              | (true, false) => Some(true)
              | (false, true) => Some(true)
              | _ => Some(false)
              }
            }
          | _ => Some(false)
          }
        }
      | _ => Some(false)
      }
    })
    ->Array.keep(x => x == Some(true))
    ->Array.length
  }
}

Js.log(readInput()->Part02.make)
