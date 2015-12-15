module IPA

open ParserUtils

type Feature =
    | Anterior
    | Aspirated
    | Back
    | Central
    | Close
    | Consonantal
    | Continuant
    | Coronal
    | DelayedRelease
    | Dental
    | Egressive
    | Glottalic
    | High
    | Lateral
    | Long
    | Low
    | Mid
    | Nasal
    | Open
    | Retroflex
    | Rounded
    | Sonorant
    | Syllabic
    | Velaric
    | Vibration
    | Voiced

type Stress =
    | Primary
    | Secondary
    | Unstressed

type Phoneme =
    {
        Representation: string;
        Features: Set<Feature>;
    }

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
        Tone: string
    }

type Word = 
    {
        Value: Syllable list;
        Annotation: string
    }

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
    
//    | '\u030C' -> Some char // Combining caron (generic rising tone)
//    | '\u0302' -> Some char // Combining circumflex (generic falling tone)

let phonemeTable =
    [
    ]
     
let parseSyllable (cs : char list) : Syllable =
    { Stress = Primary; Tone = ""; Onset = []; Rime = { Nucleus = []; Coda = [] } }

let splitSyllables (word : char list) : char list list =
    let rec splitSyllables (syllables : char list list) (buffer : char list) (word : char list) : char list list =
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

        match word with
        | [] -> List.rev ((List.rev buffer) :: syllables)
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