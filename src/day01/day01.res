open Belt

@bs.val external __dirname: string = "__dirname"

let readInput = () =>
  Node.Fs.readFileAsUtf8Sync(__dirname ++ "/input")
  ->Js.String.split("\n", _)
  ->Array.map(Belt.Int.fromString)
  ->Array.keepMap(x => x)

module Part01 = {
  let make = input => {
    let (_, pair) = input->Array.reduce(([], None), ((all, pair), a) => {
      let found = all->Array.getBy(b => 2020 - a == b)
      let newAll = Array.concat(all, [a])

      switch (found, pair) {
      | (_, Some(pair)) => (newAll, Some(pair))
      | (Some(found), _) => (newAll, Some(a, found))
      | (None, _) => (newAll, None)
      }
    })

    switch pair {
    | Some((a, b)) => a * b
    | None => -1
    }
  }
}

module Part02 = {
  let make = input => {
    let (_, pair) = input->Array.reduce(([], None), ((all, pair), a) => {
      let diff = 2020 - a
      let found = all->Array.reduce(None, (found1, b) => {
        switch found1 {
        | Some(found1) => Some(found1)
        | None => {
            let found2 = all->Array.getBy(c => diff == b + c)

            switch found2 {
            | Some(found2) => Some(b, found2)
            | None => None
            }
          }
        }
      })

      let newAll = Array.concat(all, [a])

      switch (found, pair) {
      | (_, Some(pair)) => (newAll, Some(pair))
      | (Some(b, c), _) => (newAll, Some(a, b, c))
      | (None, _) => (newAll, None)
      }
    })

    switch pair {
    | Some((a, b, c)) => a * b * c
    | None => -1
    }
  }
}

Js.log(readInput()->Part01.make)
Js.log(readInput()->Part02.make)
