module ParserUtils
open System.Text.RegularExpressions

let emptyLine (line : string) =
    if line = "" || Regex.Match(line, @"^#").Success
    then true
    else false

let removeEmptyLines (lines : List<string>) =
    List.filter (not << emptyLine) lines

let removeComment (line : string) =
    Regex.Match(line, @"^(?<keep>.*)(?:\s?#.*)?$").Groups.["keep"].Value

let trimLine (line : string) =
    line.Trim() |> removeComment