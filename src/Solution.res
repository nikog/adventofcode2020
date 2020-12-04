@bs.val external __dirname: string = "__dirname"

module type Solution = {
  let make: string => string
}

let make = (solution: module(Solution), path) => {
  let module(Solution) = solution

  Node.Path.resolve(__dirname, path)->Node.Fs.readFileAsUtf8Sync->Solution.make->Js.log
}
