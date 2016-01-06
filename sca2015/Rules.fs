module Rules

open IPA
open ParserUtils
open System.Text.RegularExpressions

type Trigger = 
    | Literal of string
    | Features of (bool * Feature) list

type Change = Change of string
type Environment = Environment of string

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
        Trigger: Trigger;
        Change: Change;
        Environment: Environment
    }

let makeTrigger s =
    let brackets = @"^\[(?<content>.+)\]$"

    let parseFeature s =
        let m = Regex.Match(s, @"^(?<b>[-+])(?<feature>.+)$")

        let parseSign s =
            match s with
            | "+" -> true
            | "-" -> false
            | _ -> failwith "Congratulations, you broke the universe."
        
        let lookupFeature s =
            let key = Map.tryFindKey (fun key value -> List.contains s key) featureTable

            match key with
            | Some key -> Map.find key featureTable
            | None -> failwithf "%A is not a valid feature." s

        if not m.Success
        then failwithf "Unable to parse %A as a feature." s
        else (parseSign m.Groups.["b"].Value, lookupFeature m.Groups.["feature"].Value)

    let makeFeatureTrigger s =
        let rsplit s : string list = Regex.Split(s, @"\s*,\s*") |> List.ofArray
        
        let makeFeatures xs =
            Features xs

        Regex.Match(s, brackets).Groups.["content"].Value
        |> rsplit
        |> List.map parseFeature
        |> makeFeatures

    match s with
    | s when Regex.Match(s, brackets).Success -> makeFeatureTrigger s
    | _ -> Literal s

let makeChange s =
    Change s

let makeEnvironment s =
    Environment s

let splitLine line =
    let m = Regex.Match(line, @"^\s*(?<trigger>.+)\s*->\s*(?<change>.+)\s*/\s*(?<environment>.+)\s*$")

    {
        Trigger = makeTrigger m.Groups.["trigger"].Value;
        Change = makeChange m.Groups.["change"].Value;
        Environment = makeEnvironment m.Groups.["environment"].Value
    }

let makeRule line =
    { Trigger = Literal ""; Change = Change ""; Environment = Environment ""}

let parseFile =
    removeEmptyLines >>
    List.map (removeComment >> makeRule)
