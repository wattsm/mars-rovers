[<AutoOpen>]
module Support

open System

[<RequireQualifiedAccess>]
module String = 

    ///Trim a string
    let trim (x : String) = x.Trim ()

    ///Split a string into lines
    let lines (x : String) = 
        x.Split (
            Environment.NewLine.ToCharArray (), 
            StringSplitOptions.RemoveEmptyEntries
        )
        |> Array.map trim

    ///Split a string into words
    let words (x : String) = 
        x.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map trim

type Outcome<'T> = 
    | Success of 'T
    | Problem of String

[<RequireQualifiedAccess>]
module Outcome = 

    ///Map one outcome to another
    let map f x = 
        match x with
        | Success y -> f y
        | Problem msg -> Problem msg

    ///Aggregate multiple inputs to an outcome
    let fold f state inputs = 
        inputs
        |> List.fold (fun state' input -> map (f input) state') state

///Bind two outcome returning functions
let bind f g = 
    fun x -> Outcome.map g (f x)

///Create a successful outcome from a value
let return' x = 
    Success x

///Infix version of bind
let (-->) = bind

