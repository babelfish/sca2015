open System.IO
open FSharp.Collections.ParallelSeq

let requireOption (options : Map<string, string>) name =
    match options.TryFind(name) with
    | Some(file) -> file
    | None -> failwith name + " required."

let readFile filePath = File.ReadAllLines(filePath)

[<EntryPoint>]
let main argv = 
    let options = List.ofSeq<string> argv |> CommandLineOptions.parseOptions
    let readOptionFile = requireOption options >> readFile >> List.ofArray

    let (inputTransformer, outputTransformer) = readOptionFile "orthography" |> Orthography.parseFile
    let lexicon = readOptionFile "lexicon" |> Lexicon.parseFile inputTransformer
    let rules = readOptionFile "rules" |> Rules.parseFile

    printfn "%A" <| SoundChangeApplier.applyRules lexicon rules

    printfn "Press any key to continue..."
    
    System.Console.ReadKey() |> ignore

    0 // return an integer exit code