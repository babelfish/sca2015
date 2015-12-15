module IPATest

open ParserUtils
open IPA
open Xunit
open FsUnit.Xunit

type ``splitSyllables tests`` () =
    [<Fact>]
    member x.``a two syllable word parses correctly`` () =
        let input = explode "foo.bar"
        let output = [['f'; 'o'; 'o']; ['b'; 'a'; 'r']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word stressed on the first syllable parses correctly`` () =
        let input = explode "ˈfoo.bar"
        let output = [['ˈ'; 'f'; 'o'; 'o']; ['b'; 'a'; 'r']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word stressed on the second syllable parses correctly`` () =
        let input = explode "fooˈbar"
        let output = [['f'; 'o'; 'o']; ['ˈ'; 'b'; 'a'; 'r']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word with an upstep parses correctly`` () =
        let input = explode "fooꜛbar"
        let output = [['f'; 'o'; 'o']; ['ꜛ'; 'b'; 'a'; 'r']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word with a downstep parses correctly`` () =
        let input = explode "fooꜜbar"
        let output = [['f'; 'o'; 'o']; ['ꜜ'; 'b'; 'a'; 'r']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word with register tones parses correctly`` () =
        let input = explode "foo˥bar˧"
        let output = [['f'; 'o'; 'o'; '˥']; ['b'; 'a'; 'r'; '˧']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word with simple contour tones parses correctly`` () =
        let input = explode "foo˧˥bar˥˧"
        let output = [['f'; 'o'; 'o'; '˧'; '˥']; ['b'; 'a'; 'r'; '˥'; '˧']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a two syllable word with complex contour tones parses correctly`` () =
        let input = explode "foo˧˨˥bar˨˥˧"
        let output = [['f'; 'o'; 'o'; '˧'; '˨'; '˥']; ['b'; 'a'; 'r'; '˨'; '˥'; '˧']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a three syllable word with primary stress on the first syllable and secondary stress on the third syllable parses correctly`` () =
        let input = explode "ˈfoo.barˌbaz"
        let output = [['ˈ'; 'f'; 'o'; 'o']; ['b'; 'a'; 'r']; ['ˌ'; 'b'; 'a'; 'z']]

        splitSyllables input |> should equal output

    [<Fact>]
    member x.``a complex word parses correctly`` () =
        let input = explode "ꜛsɑˈqɑˌqʷo:ɴ.qə"
        let output = [['ꜛ'; 's'; 'ɑ']; ['ˈ'; 'q'; 'ɑ']; ['ˌ'; 'q'; 'ʷ'; 'o'; ':'; 'ɴ']; ['q'; 'ə']]

        splitSyllables input |> should equal output