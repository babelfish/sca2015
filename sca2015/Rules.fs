module Rules

open IPA

type Rule =
    {
        Foo: string
    }

let parseFile (lines : string list) : Rule list =
    []