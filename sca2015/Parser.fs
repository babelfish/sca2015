module Parser

type Result<'a> =
    | Success of 'a * list<char>
    | Failure

type Parser<'a> = char list -> Result<'a>

let Return x =
    let p stream = Success(x, stream)
    in p

let (>>=) p f =
    let q stream =
        match p stream with
        | Success(x, xs) -> (f x) xs
        | Failure -> Failure
    in q

type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = Return y

let parse = new ParserBuilder()

/// If parser p succeeds, returns x as a result.
let (>>%) p x =
    p >>= (fun _ -> Return x)

/// Applies parsers p1 and p2, returning the result of p2.
let (>>.) p1 p2 =
    p1 >>= (fun _ -> p2)

/// Applies parsers p1 and p2, returning the result of p1.
let (.>>) p1 p2 =
    p1 >>= (fun x -> p2 >>% x)

/// Applies parsers p1 and p2, returning both results.
let (.>>.) p1 p2 =
    p1 >>= (fun x -> p2 >>= (fun y -> Return (x, y)))

let (<|>) p1 p2 =
    let p stream =
        match p1 stream with
        | Failure -> p2 stream
        | res -> res
    in p

let Optional p =
    parse {
        let! x = p

        match x with
        | Success(_, _) -> return Some x
        | Failure -> return None
    }

let rec Many p =
    parse {
        let! x = p
        let! xs = (Many p)
        return x :: xs
    } <|> Return []

let Many1 p =
    parse {
        let! x = p
        let! xs = (Many p)
        return x :: xs
    }

let Between pBegin p pEnd =
    pBegin >>. p .>> pEnd

let pChar c =
    let p stream =
        match stream with
        | x :: xs when x = c -> Success(x, xs)
        | _ -> Failure
    in p

let anyChar =
    let p stream =
        match stream with
        | x :: xs -> Success(x, xs)
        | _ -> Failure
    in p

let anyOf chars =
    let p stream =
        match stream with
        | x :: xs when Set.contains x <| Set.ofSeq chars -> Success(x, xs)
        | _ -> Failure
    in p

let noneOf chars =
    let p stream =
        match stream with
        | x :: xs when not (Set.contains x <| Set.ofSeq chars) -> Success(x, xs)
        | _ -> Failure
    in p
    
let pLetterOrDigit =
    let p stream =
        match stream with
        | x :: xs when System.Char.IsLetterOrDigit(x) -> Success(x, xs)
        | _ -> Failure
    in p