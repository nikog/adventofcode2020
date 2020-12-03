@bs.val external __dirname: string = "__dirname"

module type Solution = {
  let make: array<Js.String.t> => string
}

let make = (solution: module(Solution), path) => {
  let module(Solution) = solution

  Node.Path.resolve(__dirname, path)
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String.split("\n", _)
  ->Solution.make
  ->Js.log
}
