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

let getByRe = (str, key) =>
  Js.Re.fromString(key ++ ":(?<" ++ key ++ ">\S+)")
  ->Js.String.match_(str)
  ->Belt.Option.flatMap(x => Some(x[1]))

let parsePassport = input =>
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
    input
    ->parsePassport
    ->Belt.Array.keepMap(x => validator(x))
    ->Belt.Array.length
    ->Belt.Int.toString
  }
}

let isBetween = (val, min, max) =>
  switch Belt.Int.fromString(val) {
  | Some(val) => val >= min && val <= max
  | None => false
  }

type validity = Invalid | Valid

module Part02 = {
  let validator = pass =>
    switch pass {
    | {byr: None} => Invalid
    | {byr: Some(val)} when !(val->isBetween(1920, 2002)) => Invalid

    | {iyr: None} => Invalid
    | {iyr: Some(val)} when !(val->isBetween(2010, 2020)) => Invalid

    | {eyr: None} => Invalid
    | {eyr: Some(val)} when !(val->isBetween(2020, 2030)) => Invalid

    | {hgt: None} => Invalid
    | {hgt: Some(val)}
      when !(
        switch Js.String.substr(val, ~from=-2) {
        | "cm" => val->isBetween(150, 193)
        | "in" => val->isBetween(59, 76)
        | _ => false
        }
      ) =>
      Invalid

    | {hcl: None} => Invalid
    | {hcl: Some(val)} when !(Js.String.match_(%re("/#[0-9a-f]{6}/"), val)->Belt.Option.isSome) =>
      Invalid

    | {ecl: None} => Invalid
    | {ecl: Some(val)}
      when !(Js.String.match_(%re("/^amb|blu|brn|gry|grn|hzl|oth$/"), val)->Belt.Option.isSome) =>
      Invalid

    | {pid: None} => Invalid
    | {pid: Some(val)} when !(Js.String.match_(%re("/^\d{9}$/"), val)->Belt.Option.isSome) =>
      Invalid

    | _ => Valid
    }

  let make = input => input->parsePassport->Belt.Array.keepMap(x =>
      switch validator(x) {
      | Valid => Some(x)
      | Invalid => None
      }
    )->Belt.Array.length->Belt.Int.toString
}

Solution.make(module(Part01), "day04/input")
Solution.make(module(Part02), "day04/input")
