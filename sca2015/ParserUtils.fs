module ParserUtils
open System.Text.RegularExpressions

let emptyLine (line : string) : bool =
    line = "" || Regex.Match(line, @"^#").Success

let removeEmptyLines (lines : string list) : string list =
    List.filter (not << emptyLine) lines

let removeComment (line : string) : string =
    Regex.Match(line, @"^(?<keep>.*?)(?:\s*#.*)?$").Groups.["keep"].Value

let getAnnotatedValue (line : string) : (string * string) =
    let m = Regex.Match(line, @"^(?<keep>.*?)(?:\s*#\s*(?<annotation>.*))?$")

    (m.Groups.["value"].Value, m.Groups.["annotation"].Value)

let trimLine (line : string) : string =
    line.Trim() |> removeComment

let explode (s : string) : char list =
    [for c in s -> c]

let implode (cs : char list) =
    let sb = System.Text.StringBuilder(cs.Length)
    
    cs |> List.iter (sb.Append >> ignore)

    sb.ToString()

let listContains c = List.exists ((=) c)
    
type Result<'a> =
    | Success of 'a
    | Failure of string option