open Belt

let rec findDiffs = (~diff1Count=0, ~diff3Count=0, ~diffs=list{}, list) => {
  switch list {
  | list{} => Some(diff1Count, diff3Count, diffs)
  | list{head, ...tail} => {
      let next = tail->List.head

      switch next {
      | Some(next) =>
        let diff = next - head
        let newDiffs = diffs->List.add(diff)
        switch diff {
        | 1 => findDiffs(tail, ~diff1Count=diff1Count + 1, ~diff3Count, ~diffs=newDiffs)
        | 2 => findDiffs(tail, ~diff1Count, ~diff3Count, ~diffs=newDiffs)
        | 3 => findDiffs(tail, ~diff1Count, ~diff3Count=diff3Count + 1, ~diffs=newDiffs)
        | _ => None
        }
      | None => Some(diff1Count, diff3Count, diffs)
      }
    }
  }
}

let tribonacci = n => {
  switch n {
  | 1 => 1.
  | 2 => 2.
  | 3 => 4.
  | 4 => 7.
  | 5 => 24.
  | 6 => 44.
  | _ => 1.
  }
}

let rec combinations = (numbers, list) => {
  switch list {
  | list{head, ...tail} when head == 1 => combinations(numbers->List.add(head), tail)
  | list{_, ...tail} => tribonacci(numbers->List.length) *. combinations(list{}, tail)
  | list{} => tribonacci(numbers->List.length)
  }
}

module Part01 = {
  let make = input => {
    let content =
      input
      ->Js.String2.split("\n")
      ->Array.map(x => x->Int.fromString->Option.getUnsafe)
      ->SortArray.Int.stableSort
      ->List.fromArray

    switch findDiffs(list{0, ...content}, ~diff3Count=1) {
    | Some(diff1Count, diff3Count, _) => (diff1Count * diff3Count)->Int.toString
    | None => "0"
    }
  }
}

module Part02 = {
  let make = input => {
    let content =
      input
      ->Js.String2.split("\n")
      ->Array.map(x => x->Int.fromString->Option.getUnsafe)
      ->SortArray.Int.stableSort
      ->List.fromArray

    switch findDiffs(list{0, ...content}, ~diff3Count=1) {
    | Some(_, _, diffs) => combinations(list{}, diffs)->Float.toString
    | None => "0"
    }
  }
}

Solution.make(module(Part01), "day10/input")
Solution.make(module(Part02), "day10/input")

// let rec fact = x => {
//   switch x {
//   | 0 => 1
//   | _ => x * fact(x - 1)
//   }
// }

// let binomialCoefficient = (a, b) => fact(a) / (fact(a - b) * fact(b))

// let n = 6
// let k = n
// Js.log(binomialCoefficient(n - 1, k - 1))
// Js.log(binomialCoefficient(n - 3 - 1, k - 1))

// let n = 6
// let k = k - 1
// Js.log(binomialCoefficient(n - 1, k - 1))

// let n = n
// let k = k - 1
// Js.log(binomialCoefficient(n - 1, k - 1))

// let n = n
// let k = k - 1
// Js.log(binomialCoefficient(n - 1, k - 1))

// let n = n
// let k = k - 1
// Js.log(binomialCoefficient(n - 1, k - 1))
