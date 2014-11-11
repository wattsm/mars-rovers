module Program

open System
open System.Text

let [<Literal>] inputs = "
    5 5
    Opportunity 1 2 N
    LMLMLMLMM
    Curiousity 3 3 E
    MMRMMRMRRM
"

[<EntryPoint>]
let main _ = 

    //NOTE Let's assume that our inputs are always completely valid. Don't do this at home, kids.

    let lines = String.lines inputs
    let commands = lines.[1..]

    //Create an empty planet
    let setup () = 

        let dimensions = String.words lines.[0]
        let width = Int32.Parse dimensions.[0]
        let height = Int32.Parse dimensions.[1]

        Planet.create width height

    let commands = 
        let rec parse lines = 
            match lines with
            | [] -> []
            | rover :: movements :: rest -> (rover, movements) :: (parse rest)
            | _ -> raise (FormatException ())
        in 
            lines.[1..]
            |> Array.toList
            |> parse

    let planet = 

        //Parse the rover string and make touchdown on a planet
        let land' rover planet = 

            let splitWords (rover, planet) = 
                return' (String.words rover, planet)

            let getLocation (words : String array, planet) = 

                let x = Int32.Parse words.[1]
                let y = Int32.Parse words.[2]

                Location.create x y
                |> Outcome.map (fun location -> return' (words, location, planet))

            let getOrientation (words : String array, location, planet) = 

                let direction = Char.Parse words.[3]

                Orientation.parse direction
                |> Outcome.map (fun orientation -> return' (words, location, orientation, planet))

            let getRover (words : String array, location, orientation, planet) = 

                let name = words.[0]

                Rover.create name location orientation
                |> Outcome.map (fun rover -> return' (rover, planet))

            let touchdown (rover, planet) = 
                Planet.MissionControl.touchdown rover planet

            let f = 
                splitWords
                --> getLocation
                --> getOrientation
                --> getRover
                --> touchdown
            in f (rover, planet)

        ///Parse movement commands and pass them to the appropriate rover
        let move name (movements : String) planet = 
            movements.ToCharArray ()
            |> Array.toList
            |> Outcome.fold (fun movement planet' ->
                    match movement with
                    | 'M' -> Planet.MissionControl.instruct name Rover.move planet'
                    | 'L' -> Planet.MissionControl.instruct name Rover.left planet'
                    | 'R' -> Planet.MissionControl.instruct name Rover.right planet'
                    | _ -> raise (FormatException ())
                ) planet

        ///Create the planet and sequentially create, land and move rovers
        Outcome.fold (fun (rover, movements) planet -> 

            let name = (String.words rover).[0]

            planet
            |> land' rover
            |> move name movements

        ) (setup ()) commands

    match planet with
    | Success planet ->

        printfn "Rovers:"
        printfn "%s" (Planet.toString planet)

    | Problem message -> 
        printfn "An error was encountered:" 
        printfn "%s" message

    Console.ReadLine ()
    |> ignore

    0
    
    
