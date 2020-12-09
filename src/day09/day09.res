open Belt

let isValid = (arr, current) => {
  let (_, isValid) = arr->Array.reduce(([], false), ((checked, found), a) => {
    let isValid = checked->Array.getBy(b => current - a == b)
    let newAll = Array.concat(checked, [a])

    switch (found, isValid) {
    | (true, _) => (newAll, found)
    | (false, Some(_)) => (newAll, true)
    | (false, None) => (newAll, false)
    }
  })

  isValid
}

module Part01 = {
  let preambleLength = 25

  let make = input => {
    let rows = input->Js.String2.split("\n")->Array.map(x => x->Int.fromString->Option.getUnsafe)

    let invalidRow = rows->Array.reduceWithIndex(None, (found, row, i) => {
      if found !== None {
        found
      } else if i < preambleLength {
        None
      } else {
        let preamble = rows->Array.slice(~offset=i - preambleLength, ~len=preambleLength)

        switch isValid(preamble, row) {
        | true => None
        | false => Some(row)
        }
      }
    })

    invalidRow->Option.getExn->Int.toString
  }
}

let rec findContiguousSum = (all, numbers, sumToFind) => {
  let newArr = numbers->Array.concat([all->Array.getUnsafe(0)])
  let sum = newArr->Array.reduce(0, (sum, i) => sum + i)

  switch sum {
  | _ when sum > sumToFind => None
  | _ when sum == sumToFind => Some(numbers)
  | _ => findContiguousSum(all->Array.sliceToEnd(1), newArr, sumToFind)
  }
}

let rec loop = (rows, sumToFind) => {
  switch findContiguousSum(rows, [], sumToFind) {
  | Some(res) => res
  | None => loop(rows->Array.sliceToEnd(1), sumToFind)
  }
}

module Part02 = {
  let preambleLength = 25

  let make = input => {
    let invalidNumber = Part01.make(input)->Int.fromString->Option.getUnsafe

    let result =
      input
      ->Js.String2.split("\n")
      ->Array.map(x => x->Int.fromString->Option.getUnsafe)
      ->loop(invalidNumber)

    let min = result->Array.reduce(max_int, (min, i) => Js.Math.min_int(min, i))
    let max = result->Array.reduce(0, (min, i) => Js.Math.max_int(min, i))

    (min + max)->Int.toString
  }
}

Solution.make(module(Part01), "day09/input")
Solution.make(module(Part02), "day09/input")
