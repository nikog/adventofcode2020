open Belt

exception InvalidCommand

let parseInstruction = str => {
  let x = str->Js.String2.match_(%re("/(.+) ([+-]\d+)/"))->Option.getWithDefault([])

  switch (x[1], x[2]) {
  | (Some(op), Some(arg)) => (op, arg->Int.fromString->Option.getExn)
  | _ => raise(InvalidCommand)
  }
}

type end = Exit((int, int)) | Error((int, int, array<int>))

let rec runInstruction = (instructions, stack, (pointer, acc)) => {
  let instruction = instructions[pointer]
  let isLoop = stack->Array.getBy(x => x == pointer)

  switch (instruction, isLoop) {
  | (Some(instruction), None) => {
      let state = switch instruction {
      | ("acc", arg) => (pointer + 1, acc + arg)
      | ("jmp", arg) => (pointer + arg, acc)
      | ("nop", _) => (pointer + 1, acc)
      | _ => raise(InvalidCommand)
      }

      let stack = [pointer]->Array.concat(stack)

      runInstruction(instructions, stack, state)
    }
  | (None, _) => Exit(pointer, acc)
  | (_, _) => Error(pointer, acc, stack)
  }
}

module Part01 = {
  let make = input => {
    let instructions = input->Js.String2.split("\n")->Array.map(parseInstruction)
    let output = runInstruction(instructions, [], (0, 0))

    switch output {
    | Error(_, acc, _) | Exit(_, acc) => acc->Int.toString
    }
  }
}

let rec findCorruptedInstruction = (instructions, index) => {
  // find next jmp or nop instruction that hasn't been tested yet
  let corruptedOp = instructions->Array.reduceWithIndex(None, (found, (op, _), i) => {
    if found->Option.isNone && i >= index && (op == "nop" || op == "jmp") {
      Some(i)
    } else {
      found
    }
  })->Option.getExn

  // swap found instruction
  let newInstructions = instructions->Array.mapWithIndex((i, (op, arg)) => {
    switch op {
    | "nop" when i == corruptedOp => ("jmp", arg)
    | "jmp" when i == corruptedOp => ("nop", arg)
    | _ => (op, arg)
    }
  })

  let output = runInstruction(newInstructions, [], (0, 0))

  switch output {
  | Error(_) => findCorruptedInstruction(instructions, corruptedOp + 1)
  | Exit(_, acc) => acc->Int.toString
  }
}

module Part02 = {
  let make = input => {
    let instructions = input->Js.String2.split("\n")->Array.map(parseInstruction)

    findCorruptedInstruction(instructions, 0)
  }
}

Solution.make(module(Part01), "day08/input")
Solution.make(module(Part02), "day08/input")
