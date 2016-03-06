module IPA

open System.Collections.Generic
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
    | Advanced
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
    | Retracted
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

let validIPA =
    let set = new HashSet<char> ()

    set.Add('p') |> ignore
    set.Add('b') |> ignore
    set.Add('t') |> ignore
    set.Add('d') |> ignore
    set.Add('ʈ') |> ignore
    set.Add('ɖ') |> ignore
    set.Add('c') |> ignore
    set.Add('ɟ') |> ignore
    set.Add('k') |> ignore
    set.Add('g') |> ignore
    set.Add('q') |> ignore
    set.Add('ɢ') |> ignore
    set.Add('ʔ') |> ignore
    set.Add('m') |> ignore
    set.Add('ɱ') |> ignore
    set.Add('n') |> ignore
    set.Add('ɳ') |> ignore
    set.Add('ɲ') |> ignore
    set.Add('ŋ') |> ignore
    set.Add('ɴ') |> ignore
    set.Add('ʙ') |> ignore
    set.Add('r') |> ignore
    set.Add('ʀ') |> ignore
    set.Add('ʜ') |> ignore
    set.Add('ʢ') |> ignore
    set.Add('ɾ') |> ignore
    set.Add('ɽ') |> ignore
    set.Add('ʡ') |> ignore
    set.Add('ɸ') |> ignore
    set.Add('β') |> ignore
    set.Add('f') |> ignore
    set.Add('v') |> ignore
    set.Add('θ') |> ignore
    set.Add('ð') |> ignore
    set.Add('s') |> ignore
    set.Add('z') |> ignore
    set.Add('ʃ') |> ignore
    set.Add('ʒ') |> ignore
    set.Add('ʂ') |> ignore
    set.Add('ʐ') |> ignore
    set.Add('ç') |> ignore
    set.Add('ʝ') |> ignore
    set.Add('x') |> ignore
    set.Add('ɣ') |> ignore
    set.Add('χ') |> ignore
    set.Add('ʁ') |> ignore
    set.Add('ħ') |> ignore
    set.Add('ʕ') |> ignore
    set.Add('h') |> ignore
    set.Add('ɦ') |> ignore
    set.Add('ɬ') |> ignore
    set.Add('ɮ') |> ignore
    set.Add('ʋ') |> ignore
    set.Add('ɹ') |> ignore
    set.Add('ɻ') |> ignore
    set.Add('j') |> ignore
    set.Add('ɰ') |> ignore
    set.Add('l') |> ignore
    set.Add('ɭ') |> ignore
    set.Add('ʎ') |> ignore
    set.Add('ʟ') |> ignore
    set.Add('ʘ') |> ignore
    set.Add('ǀ') |> ignore // u01c0 dental click
    set.Add('ǃ') |> ignore // u01c3 retroflex click
    set.Add('!') |> ignore // u0021 exclamation point
    set.Add('ǂ') |> ignore
    set.Add('ǁ') |> ignore
    set.Add('¡') |> ignore
    set.Add('ʞ') |> ignore
    set.Add('ɓ') |> ignore
    set.Add('ɗ') |> ignore
    set.Add('ʄ') |> ignore
    set.Add('ɠ') |> ignore
    set.Add('ʛ') |> ignore
    set.Add('’') |> ignore // u2019 right single quote mark
    set.Add('ʼ') |> ignore // u02bc modifier apostrophe
    set.Add(''') |> ignore // u0027 apostrophe
    set.Add('ʦ') |> ignore
    set.Add('ʣ') |> ignore
    set.Add('ʧ') |> ignore
    set.Add('ʤ') |> ignore
    set.Add('ʨ') |> ignore
    set.Add('ʥ') |> ignore
    set.Add('ʍ') |> ignore
    set.Add('w') |> ignore
    set.Add('ɥ') |> ignore
    set.Add('ɫ') |> ignore
    set.Add('ɧ') |> ignore
    set.Add('i') |> ignore
    set.Add('y') |> ignore
    set.Add('ɨ') |> ignore
    set.Add('ʉ') |> ignore
    set.Add('ɯ') |> ignore
    set.Add('u') |> ignore
    set.Add('ɪ') |> ignore
    set.Add('ʏ') |> ignore
    set.Add('ʊ') |> ignore
    set.Add('e') |> ignore
    set.Add('ø') |> ignore
    set.Add('ɘ') |> ignore
    set.Add('ɵ') |> ignore
    set.Add('ɤ') |> ignore
    set.Add('o') |> ignore
    set.Add('ə') |> ignore
    set.Add('ɛ') |> ignore
    set.Add('œ') |> ignore
    set.Add('ɜ') |> ignore
    set.Add('ɞ') |> ignore
    set.Add('ʌ') |> ignore
    set.Add('ɔ') |> ignore
    set.Add('æ') |> ignore
    set.Add('ɐ') |> ignore
    set.Add('a') |> ignore
    set.Add('ɶ') |> ignore
    set.Add('ɑ') |> ignore
    set.Add('ɒ') |> ignore
    set.Add('\u0329') |> ignore // syllabic marker, combining vertical bar below
    set.Add('\u032f') |> ignore // non-syllabic marker, combining inverted breve below
    set.Add('ʰ') |> ignore
    set.Add('ⁿ') |> ignore
    set.Add('ᶿ') |> ignore
    set.Add('ᵊ') |> ignore
    set.Add('ˡ') |> ignore
    set.Add('ˣ') |> ignore
    set.Add('\u031a') |> ignore // no audible release, combining left angle above
    set.Add('\u0325') |> ignore // voiceless/slack, combining ring below
    set.Add('\u032c') |> ignore // voiced/stiff, combining caron below
    set.Add('\u0324') |> ignore // breathy, combining diaresis below
    set.Add('\u0330') |> ignore // creaky, combining tilde below
    set.Add('\u032a') |> ignore // dental, combining bridge below
    set.Add('\u033a') |> ignore // apical, combining inverted bridge below
    set.Add('\u031f') |> ignore // advanced, combining plus sign below
    set.Add('\u0320') |> ignore // retracted, combining minus sign below
    set.Add('\u0308') |> ignore // centralized, combining diaeresis
    set.Add('˔') |> ignore // raised, modifier letter up tack
    set.Add('\u031d') |> ignore // raised, combining up tack below
    set.Add('˕') |> ignore // lowered, modifier letter down tack
    set.Add('\u031e') |> ignore // lowered, combining down tack below
    set.Add('\u033c') |> ignore // linguolabial, combining seagull below
    set.Add('\u033b') |> ignore // laminal, combining square below
    set.Add('\u033d') |> ignore // mid-centralized, combining x above
    set.Add('\u0339') |> ignore // more rounded, combining right half ring below
    set.Add('\u031c') |> ignore // less rounded, combining left half ring below
    set.Add('ʷ') |> ignore
    set.Add('ʲ') |> ignore
    set.Add('ᶣ') |> ignore
    set.Add('ᶹ') |> ignore
    set.Add('ˠ') |> ignore
    set.Add('ˤ') |> ignore
    set.Add('\u0334') |> ignore // velarized/uvularized/pharyngealized, combining tilde overlay
    set.Add('\u0318') |> ignore // advanced tongue root, combining left tack below
    set.Add('\u0319') |> ignore // retracted tongue root, combining right tack below
    set.Add('\u0303') |> ignore // nasalized, combining tilde
    set.Add('\u02de') |> ignore // rhoticized, modifier letter rhotic hook
    set.Add('ɝ') |> ignore // rhotic schwa
    set.Add('ɝ') |> ignore // rhotic ɜ
    set.Add('ˈ') |> ignore // primary stress
    set.Add('ˌ') |> ignore // secondary stress
    set.Add('ː') |> ignore // long
    set.Add('ˑ') |> ignore // half-long
    set.Add('\u0306') |> ignore // short, combining breve
    set.Add('.') |> ignore // syllable break
    set.Add('⁀') |> ignore // over tie
    set.Add('‿') |> ignore // under tie
    set.Add('\u0361') |> ignore // combining double inverted breve
    set.Add('|') |> ignore // minor break
    set.Add('‖') |> ignore // major break
    set.Add('↗') |> ignore // global rise
    set.Add('↘') |> ignore // global fall
    set.Add('˥') |> ignore // very high
    set.Add('˦') |> ignore // high
    set.Add('˧') |> ignore // mid
    set.Add('˨') |> ignore // low
    set.Add('˩') |> ignore // very low
    set.Add('ꜛ') |> ignore // upstep
    set.Add('ꜜ') |> ignore // downstep
    set.Add('\u030c') |> ignore // generic rise, combining caron
    set.Add('\u0302') |> ignore // generic fall, combining circumflex
    set.Add('ǎ') |> ignore // rising a
    set.Add('ě') |> ignore // rising e
    set.Add('ǐ') |> ignore // rising i
    set.Add('ǒ') |> ignore // rising o
    set.Add('ǚ') |> ignore // rising u
    set.Add('â') |> ignore // falling a
    set.Add('ê') |> ignore // falling e
    set.Add('î') |> ignore // falling i
    set.Add('ô') |> ignore // falling o
    set.Add('û') |> ignore // falling u
    set.Add('ä') |> ignore // centralized a
    set.Add('ë') |> ignore // centralized e
    set.Add('ï') |> ignore // centralized i
    set.Add('ö') |> ignore // centralized o
    set.Add('ü') |> ignore // centralized u
    set.Add('ÿ') |> ignore // centralized y
    set.Add('ã') |> ignore // nasalized a
    set.Add('ẽ') |> ignore // nasalized e
    set.Add('ĩ') |> ignore // nasalized i
    set.Add('õ') |> ignore // nasalized o
    set.Add('ũ') |> ignore // nasalized u

    set
    

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
