module IPA

open ParserUtils

//    | '\u030C' -> Some char // Combining caron (generic rising tone)
//    | '\u0302' -> Some char // Combining circumflex (generic falling tone)

// Internal phonetic representation

type PlaceOfArticulation =
    | Labial
    | Linguolabial
    | Labiodental
    | Labiovelar
    | Dental
    | Alveolar
    | Palatoalveolar
    | Retroflex
    | Alveolopalatal
    | Palatal
    | Velar
    | Uvular
    | Pharyngeal
    | Glottal

type MannerOfArticulation =
    | Stop
    | Nasal
    | Fricative
    | Affricate
    | Flap
    | Trill
    | Approximant

type AirstreamMechanism =
    | Lingual
    | Glottalic
    | Pulmonic

type PhonationType =
    | Voiceless
    | Breathy
    | Slack
    | Modal
    | Stiff
    | Creaky
    | Glottal

type PhoneLength =
    | Short
    | Normal
    | HalfLong
    | Long
    | ExtraLong

type Rounding =
    | More
    | Less
    | Normal

type VowelBacking =
    | Front
    | NearFront
    | Central
    | NearBack
    | Back

type VowelHeight =
    | Close
    | NearClose
    | CloseMid
    | Mid
    | OpenMid
    | NearOpen
    | Open

type Feature =
    | AdditionalTargets of Phoneme list
    | AdvancedTongueRoot
    | Airstream of AirstreamMechanism
    | Aspirated
    | Continuant
    | Egressive
    | Glottalized
    | Labialized
    | Lateral
    | Length of PhoneLength
    | Lowered
    | Manner of MannerOfArticulation
    | Nasalized
    | Obstruent
    | Palatalized
    | Pharyngealized
    | Phonation of PhonationType
    | Place of PlaceOfArticulation
    | Raised
    | RetractedTongueRoot
    | Rhoticized
    | Rounded
    | Rounding of Rounding
    | Sonorant
    | Syllabic
    | Unreleased
    | Velarized
    | VowelBacking of VowelBacking
    | VowelHeight of VowelHeight

and Phoneme =
    {
        Representation: char list;
        Features: Set<Feature>;
    }

type Stress =
    | Primary
    | Secondary
    | Unstressed

type Rime =
    {
        Nucleus: Phoneme list;
        Coda: Phoneme list
    }

type Syllable = 
    {
        Stress: Stress;
        Onset: Phoneme list;
        Rime: Rime;
        Tone: char list option
    }

type Word = 
    {
        Value: Syllable list;
        Annotation: string
    }

type FeatureDetail =
    {
        PositiveModifier: char option;
        NegativeModifier: char option;
    }

let featureDetails =
    Map.empty
       .Add(Aspirated, { PositiveModifier = Some 'ʰ'; NegativeModifier = None })

// Parsing

let (|SyllableBreak|_|) (char : char) =
    match char with
    | '.' -> Some SyllableBreak
    | _ -> None
    
let (|StressMark|_|) (char : char) =
    match char with
    | 'ˈ' | 'ˌ' -> Some char
    | _ -> None

let (|BreakingToneMarkSuffix|_|) (char : char) =
    match char with
    | '˥' | '˦' | '˧' | '˨' | '˩' -> Some char
    | _ -> None
    
let (|BreakingToneMarkPrefix|_|) (char : char) =
    match char with
    | 'ꜛ' | 'ꜜ' -> Some char
    | _ -> None
     
let parseSyllable (cs : char list) : Syllable =
    { Stress = Primary; Tone = None; Onset = []; Rime = { Nucleus = []; Coda = [] } }

let splitSyllables (word : char list) : char list list =
    let rec splitSyllables (syllables : char list list) (buffer : char list) (word : char list) : char list list =
        printfn "%A %A %A" syllables buffer word

        let onBreak tail =
            match buffer with
            | [] -> splitSyllables syllables buffer tail
            | buffer -> splitSyllables ((List.rev buffer) :: syllables) [] tail

        let onPrefixBreak c tail =
            match buffer with
            | [] -> splitSyllables syllables (c :: buffer) tail
            | buffer -> splitSyllables ((List.rev buffer) :: syllables) [] (c :: tail)
            
        let onSuffixBreak c tail =
            match buffer with
            | [] ->
                let (prevSyllable, syllables) = (List.head syllables, List.tail syllables)
                splitSyllables ((c :: List.rev prevSyllable |> List.rev) :: syllables) [] tail
            | buffer -> splitSyllables ((c :: buffer |> List.rev) :: syllables) [] tail

        let finalize () =
            match buffer with
            | [] -> List.rev syllables
            | _ -> List.rev ((List.rev buffer) :: syllables)

        match word with
        | [] -> finalize ()
        | head :: tail ->
            match head with
            | SyllableBreak -> onBreak tail
            | StressMark c | BreakingToneMarkPrefix c -> onPrefixBreak c tail
            | BreakingToneMarkSuffix c -> onSuffixBreak c tail
            | c -> splitSyllables syllables (c :: buffer) tail
            
    splitSyllables [] [] word

let makeWord (word : string) : Syllable list =
    explode word |> splitSyllables |> List.map parseSyllable

let parseWord (transform : (string -> string)) (word : string) : Word =
    let (value, annotation) = getAnnotatedValue word

    { Value = transform value |> makeWord; Annotation = annotation }
