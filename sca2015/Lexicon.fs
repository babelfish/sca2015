module Lexicon

type Lexeme =
    {
        Original: string;
        Current: IPA.Word;
        Output: string option
    }

let makeLexeme (orthographyTransformer : (string -> string)) (word : string) : Lexeme =
    { Original = word; Current = IPA.parseWord orthographyTransformer word; Output = None }

let makeLexicon (orthographyTransformer : (string -> string)) : (string list -> Lexeme list) =
    List.map (makeLexeme orthographyTransformer)

let parseFile (orthographyTransformer : (string -> string)) : (string list -> Lexeme list) =
    ParserUtils.removeEmptyLines
    >> makeLexicon orthographyTransformer