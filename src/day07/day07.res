open Belt

let rec findBagsThatContainColor = (all, color) => {
  let bags = all->Array.keep(((_, contains)) => contains->Js.String2.includes(color))

  bags
  ->Array.map(((bag, _)) => bag)
  ->Array.concat(
    bags->Array.map(((bag, _)) => findBagsThatContainColor(all, bag))->Array.concatMany,
  )
}

let parseBag = str => {
  let parts = str->Js.String2.split("bags contain")

  switch (parts[0], parts[1]) {
  | (Some(bag), Some(contains)) => Some(bag->Js.String2.trim, contains)
  | _ => None
  }
}

module Part01 = {
  let make = input =>
    input
    ->Js.String2.split("\n")
    ->Array.map(parseBag)
    ->Array.keepMap(x => x)
    ->findBagsThatContainColor("shiny gold")
    ->Set.String.fromArray
    ->Set.String.size
    ->Int.toString
}

let parseBag2 = str => {
  let parts = str->Js.String2.split("bags contain")

  let contains =
    parts[1]->Option.map(x =>
      x
      ->Js.String2.split(",")
      ->Array.keepMap(x => x->Js.String2.match_(%re("/(\d) (.*) bag/")))
      ->Array.keepMap(x => {
        switch (x[1], x[2]) {
        | (Some(count), Some(color)) => Some(count->Int.fromString->Option.getWithDefault(1), color)
        | _ => None
        }
      })
    )

  switch (parts[0], contains) {
  | (Some(bag), Some(contains)) => Some(bag->Js.String2.trim, contains)
  | _ => None
  }
}

let rec findContainedBags = (all, color) => {
  let bag = all->Array.getBy(((bagColor, _)) => bagColor == color)

  switch bag {
  | Some(_, containedBags) =>
    1 +
    containedBags
    ->Array.map(((count, bagColor)) => count * findContainedBags(all, bagColor))
    ->Array.reduce(0, (sum, x) => sum + x)
  | None => 0
  }
}

module Part02 = {
  let make = input =>
    input
    ->Js.String2.split("\n")
    ->Array.map(parseBag2)
    ->Array.keepMap(x => x)
    ->findContainedBags("shiny gold")
    ->(x => x - 1)
    ->Int.toString
}

Solution.make(module(Part01), "day07/input")
Solution.make(module(Part02), "day07/input")
