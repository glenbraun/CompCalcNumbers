// Counting in Arabic in binary, octal, decimal, and hexedecimal
// and in Roman numerals
// Glen Braun, 2018

type Sign =
    | Positive
    | Negative

type ICountable =
    abstract member Increment : unit -> unit
    abstract member Decrement : unit -> unit
    abstract member Add : ICountable -> unit
    abstract member Subtract : ICountable -> unit
    abstract member Sign : unit -> Sign
    abstract member IsZero : unit -> bool
    abstract member Copy : unit -> ICountable
    abstract member ToInt : unit -> int

type Quantity<'a when 'a : equality> (f:(int -> int -> 'a array), sign:Sign, value: 'a array) =
    let mutable CurrentSign = sign
    let mutable CurrentValue = value

    let rec increase (f:int -> int -> 'a array) (cv:'a array) pos =
        let symbols = f (cv.Length) pos

        match (cv.[pos]) with
        | x when x = symbols.[symbols.Length - 1] -> 
            // for example, in Arabic base 10, this would match "9"
            cv.[pos] <- symbols.[0]

            if pos = 0 then
                let symbols = f (cv.Length + 1) pos
                Array.append ([| symbols.[1] |]) cv
            else
                increase f cv (pos-1)

        | x ->
            let indexOfX = Array.findIndex (x.Equals) symbols
            cv.[pos] <- symbols.[indexOfX+1]
            cv

    let rec decrease (f:int -> int -> 'a array) (cs:Sign) (cv:'a array) pos =
        let symbols = f (cv.Length) pos

        match (cv.[pos]) with
        | x when x = symbols.[0] -> 
            if (cv.Length) = 1 then
                cv.[pos] <- symbols.[1]
                if cs = Sign.Negative then
                    (Sign.Positive, cv)
                else
                    (Sign.Negative, cv)
            else
                cv.[pos] <- symbols.[symbols.Length - 1]
                decrease f cs cv (pos-1)
        | x when x = symbols.[1] ->
            if pos = 0 && cv.Length > 1 then
                (cs, Array.sub cv 1 (cv.Length - 1))
            else
                cv.[pos] <- symbols.[0]
                if cv.Length = 1 && cs = Sign.Negative then
                    (Sign.Positive, cv)
                else
                    (cs, cv)
        | x ->
            let indexOfX = Array.findIndex (x.Equals) symbols
            cv.[pos] <- symbols.[indexOfX-1]
            (cs, cv)

    override this.ToString() = 
        let ssign = 
            match CurrentSign with
            | Sign.Positive -> ""
            | Sign.Negative -> "-"

        Array.fold (fun s v -> s + v.ToString()) ssign CurrentValue

    interface ICountable with
        member this.Increment() =
            match CurrentSign with
            | Sign.Positive ->
                CurrentValue <- increase f (CurrentValue) (CurrentValue.Length - 1)
            | Sign.Negative ->
                let (ns, nv) = decrease f CurrentSign CurrentValue (CurrentValue.Length - 1)
                CurrentSign <- ns
                CurrentValue <- nv

        member this.Decrement() = 
            match CurrentSign with
            | Sign.Positive ->
                let (ns, nv) = decrease f CurrentSign CurrentValue (CurrentValue.Length - 1)
                CurrentSign <- ns
                CurrentValue <- nv
            | Sign.Negative ->
                CurrentValue <- increase f (CurrentValue) (CurrentValue.Length - 1)

        member this.Add(x) = 
            let y = x.Copy()
            while not(y.IsZero()) do
                match (y.Sign()) with
                | Sign.Positive ->
                    y.Decrement()
                    (this :> ICountable).Increment()
                | Sign.Negative ->
                    y.Increment()
                    (this :> ICountable).Decrement()

        member this.Subtract(x) = 
            let y = x.Copy()
            while not(y.IsZero()) do
                match (y.Sign()) with
                | Sign.Positive ->
                    y.Decrement()
                    (this :> ICountable).Decrement()
                | Sign.Negative ->
                    y.Increment()
                    (this :> ICountable).Increment()

        member this.Sign() = 
            CurrentSign

        member this.IsZero() = 
            if CurrentValue.Length = 1 then
                let symbols = f 1 0
                (CurrentValue.[0] = symbols.[0])
            else
                false

        member this.Copy() = 
            (new Quantity<'a>(f, CurrentSign, Array.copy CurrentValue)) :> ICountable

        member this.ToInt() =
            let y = (this :> ICountable).Copy()
            match y.Sign() with
            | Sign.Positive ->
                let mutable i = 0
                while not(y.IsZero()) do
                    y.Decrement()
                    i <- i + 1
                i
            | Sign.Negative ->
                let mutable i = 0
                while not(y.IsZero()) do
                    y.Increment()
                    i <- i + 1
                -1 * i

let Decimal (i:int) =
    let fDecimal (length:int) (pos:int) = 
        "0123456789".ToCharArray()

    let n = Quantity<char>(fDecimal, Sign.Positive, "0".ToCharArray()) :> ICountable
    if i >= 0 then
        for x = 1 to i do
            n.Increment()
    else
        for x = -1 downto i do
            n.Decrement()
    n

let Binary (i:int) = 
    let fBinary (length:int) (pos:int) =
        "01".ToCharArray()

    let n = Quantity<char>(fBinary, Sign.Positive, "0".ToCharArray()) :> ICountable
    let d = Decimal i
    n.Add(d)
    n
    
let Octal (i:int) =
    let fOctal (length:int) (pos:int) =
        "01234567".ToCharArray()
    
    let n = Quantity<char>(fOctal, Sign.Positive, "0".ToCharArray()) :> ICountable
    let d = Decimal i
    n.Add(d)
    n

let Hexadecimal (i:int) =
    let fHexaDecimal (length:int) (pos:int) = 
        "0123456789ABCDEF".ToCharArray()

    let n = Quantity<char>(fHexaDecimal, Sign.Positive, "0".ToCharArray()) :> ICountable
    let d = Decimal i
    n.Add(d)
    n

let Roman (i:int) =
    let fRoman (length:int) (pos:int) =
        // Symbol I, V, X,  L,  C,   D,   M
        // Value  1, 5, 10, 50, 100, 500, 1,000
        match length - pos with
        | 1 -> [| ""; "I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX" |]  // ones
        | 2 -> [| ""; "X"; "XX"; "XXX"; "XL"; "L"; "LX"; "LXX"; "LXXX"; "XC" |]  // tens
        | 3 -> [| ""; "C"; "CC"; "CCC"; "CD"; "D"; "DC"; "DCC"; "DCCC"; "CM" |]  // hundreds
        | 4 -> [| ""; "M"; "MM"; "MMM"; "MMMM" |]  // thousands
        | _ -> failwith "Too big for Roman numerals"

    let n = Quantity<string>(fRoman, Sign.Positive, [| "" |]) :> ICountable
    let d = Decimal i
    n.Add(d)
    n
  

[<EntryPoint>]
let main argv =
    let roman = (Roman 200)
    
    let binary10 = Binary 10
    let rc = roman.Copy()
    roman.Subtract(binary10)
    printfn "[Roman %s (%d)] - [Binary %s (%d)] = [Roman %s (%d)]" (rc.ToString()) (rc.ToInt()) (binary10.ToString()) (binary10.ToInt()) (roman.ToString()) (roman.ToInt())
    // [Roman CC (200)] - [Binary 1010 (10)] = [Roman CXC (190)]

    let octal448 = Octal 448
    let rc = roman.Copy()
    roman.Add(octal448)
    printfn "[Roman %s (%d)] + [Octal %s (%d)] = [Roman %s (%d)]" (rc.ToString()) (rc.ToInt()) (octal448.ToString()) (octal448.ToInt()) (roman.ToString()) (roman.ToInt())
    // [Roman CXC (190)] + [Octal 700 (448)] = [Roman DCXXXVIII (638)]

    let decimalNeg325 = Decimal -325
    let rc = roman.Copy()
    roman.Add(decimalNeg325)
    printfn "[Roman %s (%d)] + [Decimal %s (%d)] = [Roman %s (%d)]" (rc.ToString()) (rc.ToInt()) (decimalNeg325.ToString()) (decimalNeg325.ToInt()) (roman.ToString()) (roman.ToInt())
    // [Roman DCXXXVIII (638)] + [Decimal -325 (-325)] = [Roman CCCXIII (313)]

    let hex93 = Hexadecimal 93
    let rc = roman.Copy()
    roman.Subtract(hex93)
    printfn "[Roman %s (%d)] + [Hexadecimal %s (%d)] = [Roman %s (%d)]" (rc.ToString()) (rc.ToInt()) (hex93.ToString()) (hex93.ToInt()) (roman.ToString()) (roman.ToInt())
    // [Roman CCCXIII (313)] + [Hexadecimal 5D (93)] = [Roman CCXX (220)]
    
    printfn "[Roman %s (%d)]" (roman.ToString()) (roman.ToInt())
    // [Roman CCXX (220)]
    
    0
