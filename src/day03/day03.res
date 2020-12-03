open Belt

module Part01 = {
  let pickTrees = ((trees, _)) => trees

  let make = input => {
    input->Array.reduce((0, 0), ((trees, x), row) => {
      let rowArr = Js.String2.split(row, "")
      let length = Array.length(rowArr)
      let coord = x < length ? x : x - length
      let pos = rowArr[coord]

      switch pos {
      | Some(pos) => (trees + (pos == "#" ? 1 : 0), coord + 3)
      | None => (trees, x)
      }
    })->pickTrees->Int.toString
  }
}

module Part02 = {
  let deltas = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let pickTrees = ((trees, _)) => trees

  let make = input => {
    deltas->Array.map(((deltaX, deltaY)) => {
      input->Array.reduceWithIndex((0., (0, 0)), ((trees, (x, y)), row, index) => {
        let rowArr = Js.String2.split(row, "")
        let pos = rowArr[mod(x, Array.length(rowArr))]

        switch (pos, y == index) {
        | (Some(pos), true) => (trees +. (pos == "#" ? 1. : 0.), (x + deltaX, y + deltaY))
        | _ => (trees, (x, y))
        }
      })
    })->Array.reduce(1., (all, (trees, _)) => all *. trees)->Float.toString
  }
}

Solution.make(module(Part01), "day03/input")
Solution.make(module(Part02), "day03/input")
