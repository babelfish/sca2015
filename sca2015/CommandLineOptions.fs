module CommandLineOptions
open System.Text.RegularExpressions

let ensureValuePresence optionName tail =
    match tail with
    | [] -> failwithf "Failed to provide value for %s" optionName
    | _ -> tail.Head

let extractValue optionName head tail =
    let m = Regex.Match(head, @"^--.+=(?<value>.+)$")

    if m.Success
    then m.Groups.["value"].Value
    else ensureValuePresence optionName tail

let rec findOptionValue args (optionName, flagMatch) = 
    match args with
    | head :: tail -> if flagMatch head
                      then (optionName, Some(extractValue optionName head tail))
                      else findOptionValue tail (optionName, flagMatch)
    | [] -> (optionName, None)

let rec findOptionPresence args (optionName, flagMatch) =
    match args with
    | head :: tail -> if flagMatch head
                      then (optionName, Some(optionName))
                      else findOptionPresence tail (optionName, flagMatch)
    | [] -> (optionName, None)

let parseOption args (optionName, shortFlag, finder) =
    let longPattern = "^--" + optionName + "=.+$"

    finder args (optionName, (fun arg -> arg = "-" + shortFlag || Regex.Match(arg, longPattern).Success))

let availableOptions =
    [
        ("orthography", "o", findOptionValue);
        ("lexicon", "l", findOptionValue);
        ("rules", "r", findOptionValue);
        ("format", "f", findOptionValue)
    ]

let parseOptions (args : List<string>) =
    List.map (parseOption args) availableOptions
    |> List.choose (fun (optionName, value) ->
        match value with
        | Some(str) -> Some((optionName, str))
        | None -> None
    )
    |> Map.ofList