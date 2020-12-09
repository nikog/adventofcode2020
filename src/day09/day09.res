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

let findInvalidRow = (rows, preambleLength) => {
  rows->Array.reduceWithIndex(None, (found, row, i) => {
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
}

module Part01 = {
  let preambleLength = 25

  let make = input => {
    let rows = input->Js.String2.split("\n")->Array.map(x => x->Int.fromString->Option.getUnsafe)
    let invalidRow = findInvalidRow(rows, preambleLength)
    invalidRow->Option.getExn->Int.toString
  }
}

let rec findContiguousSum = (all, numbers, sumToFind) => {
  let newArr = list{all->List.headExn, ...numbers}
  let sum = newArr->List.reduce(0, (sum, i) => sum + i)

  switch sum {
  | _ when sum > sumToFind => None
  | _ when sum == sumToFind => Some(numbers)
  | _ => findContiguousSum(all->List.tailExn, newArr, sumToFind)
  }
}

let rec loop = (rows, sumToFind) => {
  let res = findContiguousSum(rows, list{}, sumToFind)

  switch res {
  | Some(res) => res
  | None => loop(rows->List.tailExn, sumToFind)
  }
}

module Part02 = {
  let preambleLength = 25

  let make = input => {
    let rows = input->Js.String2.split("\n")->Array.map(x => x->Int.fromString->Option.getUnsafe)
    let invalidNumber = findInvalidRow(rows, preambleLength)->Option.getExn

    let result = rows->List.fromArray->loop(invalidNumber)

    let min = result->List.reduce(max_int, (min, i) => Js.Math.min_int(min, i))
    let max = result->List.reduce(0, (max, i) => Js.Math.max_int(max, i))

    (min + max)->Int.toString
  }
}

Solution.make(module(Part01), "day09/input")
Solution.make(module(Part02), "day09/input")
