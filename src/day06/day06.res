open Belt

module Part01 = {
  let uniqueAnswersLength = group =>
    group
    ->Js.String2.replaceByRe(%re("/\\n/g"), "")
    ->Js.String2.split("")
    ->Set.String.fromArray
    ->Set.String.size

  let make = input =>
    input
    ->Js.String2.split("\n\n")
    ->Array.map(uniqueAnswersLength)
    ->Array.reduce(0, (sum, x) => sum + x)
    ->Int.toString
}

module Part02 = {
  let commonAnswersLength = group =>
    group
    ->Js.String2.split("\n")
    ->Array.map(str => str->Js.String2.split("")->Set.String.fromArray)
    ->Array.reduce(None, (prev, curr) =>
      prev->Option.mapWithDefault(Some(curr), prev => prev->Set.String.intersect(curr)->Some)
    )
    ->Option.mapWithDefault(0, intersection => intersection->Set.String.size)

  let make = input =>
    input
    ->Js.String2.split("\n\n")
    ->Array.map(commonAnswersLength)
    ->Array.reduce(0, (sum, x) => sum + x)
    ->Int.toString
}

Solution.make(module(Part01), "day06/input")
Solution.make(module(Part02), "day06/input")
