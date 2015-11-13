module Lexicon

type Lexeme =
    {
        Original: string;
        Current: IPA.Word;
        Output: Option<string>
    }

let parseFile =
    ParserUtils.removeEmptyLines
    >> List.map (ParserUtils.removeComment >> IPA.parseWord)