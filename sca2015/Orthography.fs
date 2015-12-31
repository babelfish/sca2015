module Orthography

open System.Text.RegularExpressions
open ParserUtils

type Orthography =
    {
        InputMap: (string * string) list;
        OutputMap: (string * string) list
    }
    ~
let split (line : string) : (string * string) =
    let m = Regex.Match(line, @"^(?<input>.+:\s*(?<output>.+)$")

    (m.Groups.["input"].Value, m.Groups.["output"].Value)

let rec parseSection (lines : string list) : (string * string) list =
    match lines with
    | [] -> []
    | head :: tail -> if Regex.Match(head, @"^\[.+\]$").Success
                      then []
                      else split head :: parseSection tail

let rec findSection (header : string) (lines : string list) : (string * string) list =
    match lines with
    | [] -> []
    | head :: tail -> if head = header
                      then parseSection tail
                      else findSection header tail

let makeOrthography (lines : string list) : Orthography =
    {
        InputMap = findSection "[Script to IPA]" lines;
        OutputMap = findSection "[IPA to Script]" lines
    }

let rec makeTransformer (map : (string * string) list) (s : string) : string =
    match map with
    | [] -> s
    | (input, output) :: tail -> makeTransformer tail <| s.Replace(input, output)

let makeTransformers (orthography : Orthography) : ((string -> string) * (string -> string)) =
    (makeTransformer orthography.InputMap, makeTransformer orthography.OutputMap)

let parseFile : string list -> ((string -> string) * (string -> string)) =
    removeEmptyLines
    >> List.map removeComment
    >> makeOrthography
    >> makeTransformers