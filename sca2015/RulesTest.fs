module RulesTest

open Xunit
open FsUnit.Xunit
open Rules
open IPA

type ``parseFile test`` () =
    let fileContents = 
        [ "-- A comment only line"
        ; "a -> e / _ -- a simple direct substitution"
        ; "a -> e / #_ -- a word initial substitution"
        ; "a -> e / _# -- a word final substitution"
        ; "[+atr] -> [-atr] / _ -- simple removal of a feature"
        ; "[+velar] -> [+palatal] / _ -- addition of a feature"
        ; "[+close +front +round] -> [-round] / _ -- multiple trigger features"
        ; "[+close +front +round] -> [-round +back] / _ -- multiple trigger and change features"
        ; "[+velar][+palatal] -> [+palatal][+velar] / _ -- multiple feature sets"
        ; "ae -> ea / _ -- multiple literals"
        ]
    
    let output =
        [ { Trigger = [Literal "a"]; Change = [Literal "e"]; Environment = Environment "_"}
        ; { Trigger = [Literal "a"]; Change = [Literal "e"]; Environment = Environment "#_"}
        ; { Trigger = [Literal "a"]; Change = [Literal "e"]; Environment = Environment "_#"}
        ; { Trigger = [FeatureSet [(true, AdvancedTongueRoot)]]; Change = [FeatureSet [(false, AdvancedTongueRoot)]]; Environment = Environment "_"}
        ; { Trigger = [FeatureSet [(true, Place Velar)]]; Change = [FeatureSet [(true, Place Palatal)]]; Environment = Environment "_"}
        ; { Trigger = [FeatureSet [(true, VowelHeight Close); (true, VowelBacking Front); (true, Rounded)]]; Change = [FeatureSet [(false, Rounded)]]; Environment = Environment "_"}
        ; { Trigger = [FeatureSet [(true, VowelHeight Close); (true, VowelBacking Front); (true, Rounded)]]; Change = [FeatureSet [(false, Rounded); (true, VowelBacking Back)]]; Environment = Environment "_"}
        ; { Trigger = [FeatureSet [(true, Place Velar)]; FeatureSet [(true, Place Palatal)]]; Change = [FeatureSet [(true, Place Palatal)]; FeatureSet [(true, Place Velar)]]; Environment = Environment "_"}
        ; { Trigger = [Literal "ae"]; Change = [Literal "ea"]; Environment = Environment "_" }
        ]
                
    [<Fact>]
    member x.``parseFile transforms the input file into a list of Rules`` () =
        parseFile fileContents |> should equal output