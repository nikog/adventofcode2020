type passport = {
  byr: option<string>,
  iyr: option<string>,
  eyr: option<string>,
  hgt: option<string>,
  hcl: option<string>,
  ecl: option<string>,
  pid: option<string>,
  cid: option<string>,
}

type status = Valid | Invalid

let getByRe = (str, key) => {
  let re = Js.Re.fromString(key ++ ":(?<" ++ key ++ ">\S+)")
  let result = Js.String.match_(re, str)

  switch result {
  | Some(result) => Some(result[1])
  | None => None
  }
}

let parsePassport = input => {
  input->Js.String2.split("\n\n")->Belt.Array.map(getByRe)->Belt.Array.map(getByRe => {
    byr: getByRe("byr"),
    iyr: getByRe("iyr"),
    eyr: getByRe("eyr"),
    hgt: getByRe("hgt"),
    hcl: getByRe("hcl"),
    ecl: getByRe("ecl"),
    pid: getByRe("pid"),
    cid: getByRe("cid"),
  })
}

let getResult = passport =>
  passport->Belt.Array.keepMap(x => x)->Belt.Array.length->Belt.Int.toString

module Part01 = {
  let validator = pass => {
    switch pass {
    | {
        byr: Some(_),
        iyr: Some(_),
        eyr: Some(_),
        hgt: Some(_),
        hcl: Some(_),
        ecl: Some(_),
        pid: Some(_),
        cid: Some(_) | None,
      } =>
      Some(pass)
    | _ => None
    }
  }

  let make = input => {
    input->parsePassport->Belt.Array.map(validator)->getResult
  }
}

let isBetween = (val, min, max) =>
  switch Belt.Int.fromString(val) {
  | Some(val) => val >= min && val <= max
  | None => false
  }

module Part02 = {
  let validator = pass => {
    switch pass {
    | {byr: Some(val)} when !(val->isBetween(1920, 2002)) => None
    | {iyr: Some(val)} when !(val->isBetween(2010, 2020)) => None
    | {eyr: Some(val)} when !(val->isBetween(2020, 2030)) => None
    | {hgt: Some(val)}
      when !(
        switch Js.String.substr(val, ~from=-2) {
        | "cm" => val->isBetween(150, 193)
        | "in" => val->isBetween(59, 76)
        | _ => false
        }
      ) =>
      None
    | {hcl: Some(val)} when !(Js.String.match_(%re("/#[0-9a-f]{6}/"), val)->Belt.Option.isSome) =>
      None
    | {ecl: Some(val)}
      when !(Js.String.match_(%re("/^amb|blu|brn|gry|grn|hzl|oth$/"), val)->Belt.Option.isSome) =>
      None
    | {pid: Some(val)} when !(Js.String.match_(%re("/^\d{9}$/"), val)->Belt.Option.isSome) => None
    | {
        byr: Some(_),
        iyr: Some(_),
        eyr: Some(_),
        hgt: Some(_),
        hcl: Some(_),
        ecl: Some(_),
        pid: Some(_),
        cid: Some(_) | None,
      } =>
      Some(pass)
    | _ => None
    }
  }

  let make = input => {
    input->parsePassport->Belt.Array.map(validator)->getResult
  }
}

Solution.make(module(Part01), "day04/input")
Solution.make(module(Part02), "day04/input")
