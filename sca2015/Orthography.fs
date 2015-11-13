module Orthography

open System.Text.RegularExpressions
open ParserUtils

type Orthography =
    {
        InputMap: List<(string * string)>;
        OutputMap: List<(string * string)>
    }

let split (line : string) =
    let m = Regex.Match(line, @"^(?<input>.+:\s*(?<output>.+)$")

    (m.Groups.["input"].Value, m.Groups.["output"].Value)

let rec findInputPairs (lines : List<string>) =
    match lines with
    | [] -> []
    | head :: tail -> if head = "[IPA to Script]"
                      then []
                      else split head :: findInputPairs tail

let rec findInput (lines : List<string>) =
    match lines with
    | [] -> []
    | head :: tail -> if head = "[Script to IPA]"
                      then findInputPairs tail
                      else findInput tail

let rec findOutputPairs (lines : List<string>) =
    match lines with
    | [] -> []
    | head :: tail -> if head = "[Script to IPA]"
                      then []
                      else split head :: findOutputPairs tail

let rec findOutput (lines : List<string>) =
    match lines with
    | [] -> []
    | head :: tail -> if head = "[IPA to Script]"
                      then findOutputPairs tail
                      else findOutput tail

let makeOrthography (lines : List<string>) =
    {
        InputMap = findInput lines;
        OutputMap = findOutput lines
    }

let rec makeTransformer (map : List<(string * string)>) (s : string) =
    match map with
    | [] -> s
    | (input, output) :: tail -> makeTransformer tail <| s.Replace(input, output)

let makeTransformerMap (map : List<(string * string)>) =
    List.map <| makeTransformer map

let makeTransformers (orthography : Orthography) =
    (makeTransformerMap orthography.InputMap, makeTransformerMap orthography.OutputMap)

let parseFile =
    removeEmptyLines
    >> List.map removeComment
    >> makeOrthography
    >> makeTransformers