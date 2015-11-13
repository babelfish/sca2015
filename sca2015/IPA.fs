module IPA

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
    | FallingRisingTone
    | FallingTone
    | Glottalic
    | High
    | HighTone
    | Lateral
    | Long
    | Low
    | LowTone
    | Mid
    | Nasal
    | Open
    | Retroflex
    | RisingFallingTone
    | RisingTone
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

type Rime =
    {
        Nucleus: List<string>;
        Coda: List<string>
    }

type Syllable = 
    {
        Stress: Stress;
        Onset: List<string>;
        Rime: Rime
    }

type Word = 
    {
        Value: List<Syllable>;
        Annotation: string
    }

let phonemeTable =
    [
    ]

let parseWord (word : string) =
    { Value = []; Annotation = "" }