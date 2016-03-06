module Rules

open IPA
open Parser
open ParserUtils
open System.Text.RegularExpressions

type RuleFeatures = 
    | Literal of string * string option
    | FeatureSet of (bool * Feature) list * string option

let featureTable =
    Map.empty
       .Add(["atr"], AdvancedTongueRoot)
       .Add(["ling"; "lingual"], Airstream Lingual)
       .Add(["pul"; "pulmonic"], Airstream Pulmonic)
       .Add(["glottalic"], Airstream Glottalic)
       .Add(["asp"; "aspirated"], Aspirated)
       .Add(["cont"; "continuant"], Continuant)
       .Add(["egr"; "egressive"], Egressive)
       .Add(["glottalized"], Glottalized)
       .Add(["labialized"], Labialized)
       .Add(["lat"; "lateral"], Lateral)
       .Add(["short"], Length Short)
       .Add(["halflong"], Length HalfLong)
       .Add(["long"], Length Long)
       .Add(["extralong"], Length ExtraLong)
       .Add(["low"; "lowered"], Lowered)
       .Add(["stop"; "plosive"], Manner Stop)
       .Add(["nas"; "nasal"], Manner Nasal)
       .Add(["fric"; "fricative"], Manner Fricative)
       .Add(["aff"; "affricate"], Manner Affricate)
       .Add(["flap"; "tap"], Manner Flap)
       .Add(["trill"], Manner Trill)
       .Add(["app"; "approx"; "approximant"], Manner Approximant)
       .Add(["less-round"], Rounding Less)
       .Add(["more-round"], Rounding More)
       .Add(["nasalized"], Nasalized)
       .Add(["obs"; "obstruent"], Obstruent)
       .Add(["palatalized"], Palatalized)
       .Add(["pharyngealized"], Pharyngealized)
       .Add(["breathy"], Phonation Breathy)
       .Add(["slack"], Phonation Slack)
       .Add(["voi"; "voice"; "voiced"], Phonation Modal)
       .Add(["stiff"], Phonation Stiff)
       .Add(["creaky"], Phonation Creaky)
       .Add(["glottalvoice"], Phonation Glottal)
       .Add(["lab"; "labial"], Place Labial)
       .Add(["linguolabial"], Place Linguolabial)
       .Add(["labiodental"], Place Labiodental)
       .Add(["labiovelar"], Place Labiovelar)
       .Add(["dent"; "dental"], Place Dental)
       .Add(["alv"; "alveolar"], Place Alveolar)
       .Add(["palatoalveolar"], Place Palatoalveolar)
       .Add(["ret"; "retroflex"], Place Retroflex)
       .Add(["alveopalatal"], Place Alveolopalatal)
       .Add(["pal"; "palatal"], Place Palatal)
       .Add(["vel"; "velar"], Place Velar)
       .Add(["uvu"; "uvular"], Place Uvular)
       .Add(["phar"; "pharyngeal"], Place Pharyngeal)
       .Add(["glot"; "glottal"], Place PlaceOfArticulation.Glottal)
       .Add(["raised"], Raised)
       .Add(["rtr"], RetractedTongueRoot)
       .Add(["rhot"; "rhotic"; "rhoticized"], Rhoticized)
       .Add(["round"; "rounded"], Rounded)
       .Add(["son"; "sonorant"], Sonorant)
       .Add(["syl"; "syllabic"], Syllabic)
       .Add(["unreleased"], Unreleased)
       .Add(["velarized"], Velarized)
       .Add(["front"], VowelBacking Front)
       .Add(["near-front"], VowelBacking NearFront)
       .Add(["central"], VowelBacking Central)
       .Add(["near-back"], VowelBacking NearBack)
       .Add(["back"], VowelBacking Back)
       .Add(["close"], VowelHeight Close)
       .Add(["near-close"], VowelHeight NearClose)
       .Add(["close-mid"], VowelHeight CloseMid)
       .Add(["mid"], VowelHeight Mid)
       .Add(["open-mid"], VowelHeight OpenMid)
       .Add(["near-open"], VowelHeight NearOpen)
       .Add(["open"], VowelHeight Open)

type Rule =
    {
        Trigger: RuleFeatures list;
        Change: RuleFeatures list;
        Environment: string;
        Annotation: string option
    }

let IPAParser =
    let p stream =
        match stream with
        | x :: xs when validIPA.Contains(x) -> Success(x, xs)
        | _ -> Failure
    in p

let SignParser = (pChar '+' >>% true) <|> (pChar '-' >>% false)

let NonSignParser =
    noneOf ['+'; '-']

let lookupFeature s =
    let key = Map.tryFindKey (fun key value -> List.contains s key) featureTable

    match key with
    | Some key -> Map.find key featureTable
    | None -> failwithf "%A is not a valid feature." s

let AnnotationParser =
    parse {
        let! annotation = Many1 pLetterOrDigit .>> pChar ':'

        return annotation
    }

let FeatureSetParser =
    let featuresParser =
        Many1 (parse {
            let! s = SignParser
            let! cs = Many1 NonSignParser
                
            return (s, implode cs |> lookupFeature)
        }) 

    parse {
        let! features = pChar '[' >>. featuresParser .>> pChar ']'

        return features
    }

let LiteralParser =
    parse {
        let! literal = Many1 IPAParser

        return literal
    }

let FeatureParser =
    parse {
        let! feature = FeatureSetParser <|> LiteralParser

        return feature
    }
    

let FeaturesParser =
    parse {
        let! features = Many1 FeatureParser

        return features
    }

let parseFeatures s =
    match FeaturesParser <| explode s with
    | Success(x, xs) -> x
    | Failure -> failwithf "Failed to parse %A as a valid list of features." s

let RuleParser =
    parse {
        let! trigger = TriggerParser
        let! change = ChangeParser
        let! environment = EnvironmentParser
        let! annotation = CommentParser
        
        return { Trigger = trigger; Change = change; Environment = environment; Annotation = annotation }
    }

let makeRule line =
    let m = line =~ @"^(?<trigger>.+?)->(?<change>.+?)/(?<environment>.+?)$"

    {
        Trigger =  m.Groups.["trigger"].Value |> parseFeatures;
        Change =  m.Groups.["change"].Value |> parseFeatures;
        Environment = m.Groups.["environment"].Value;
        Annotation = None
    }

let parseFile =
    removeEmptyLines >>
    List.map (removeComment >> stripWhitespace >> makeRule)
