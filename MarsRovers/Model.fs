[<AutoOpen>]
module Model

open System

type Direction =
    | North = 0
    | East = 1
    | South = 2
    | West = 3

[<RequireQualifiedAccess>]
module Location = 

    type Point = {
        X : int;
        Y : int;
    }

    type Instance = Location of Point

    ///Create a new location
    let create x y = 
        return' (Location { X = x; Y = y; })

    ///Access point with f
    let private apply f (Location point) = f point

    ///Mutate using f
    let private mutate f = apply (fun point -> Location (f point))

    ///Move in the given direction
    let move direction = mutate (fun point ->
        match direction with
        | Direction.North -> { point with Y = (point.Y + 1); }
        | Direction.East -> { point with X = (point.X + 1); }
        | Direction.South -> { point with Y = (point.Y - 1); }
        | Direction.West -> { point with X = (point.X - 1); }
    )

    ///Convert to a string representation
    let toString = apply (fun point ->
        sprintf "(%u, %u)" point.X point.Y
    )

    ///Convert to an X, Y tuple representation
    let toTuple = apply (fun point -> (point.X, point.Y))

[<RequireQualifiedAccess>]
module Orientation = 

    type Instance = Orientation of Direction

    ///Parse an orientation from a character
    let parse = 

        let parseDirection char = 
            match char with
            | 'N' -> Success (Direction.North)
            | 'E' -> Success (Direction.East)
            | 'S' -> Success (Direction.South)
            | 'W' -> Success (Direction.West)
            | _ -> Problem ("The cardinal point character must be N, E, S or W.")

        let create direction = 
            return' (Orientation direction)

        parseDirection --> create

    ///Access direction using f
    let private apply f (Orientation direction) = f direction   

    ///Mutate using f
    let private mutate f = apply (fun direction -> 
        Orientation (f direction)
    )

    ///Get the direction
    let direction = apply id

    ///Rotate left
    let left = mutate (fun direction ->
        match (int direction) with
        | 0 -> Direction.West
        | x -> enum<Direction> (x - 1)
    )

    ///Rotate right
    let right = mutate (fun direction ->
        match (int direction) with
        | 3 -> Direction.North
        | x -> enum<Direction> (x + 1)
    )

    ///Convert to a string represetnation
    let toString = apply (fun direction ->
        match direction with
        | Direction.North -> "North"
        | Direction.East -> "East"
        | Direction.South -> "South"
        | Direction.West -> "West"
    )

[<RequireQualifiedAccess>]
module Rover = 

    type State = {
        Name : String;
        Location : Location.Instance;
        Orientation : Orientation.Instance;
    }

    type Instance = Rover of State

    ///Create a new rover
    let create name location orientation = 
        if (String.IsNullOrWhiteSpace name) then
            Problem "The rover name cannot be null, empty or whitespace."
        else
            let rover = Rover { Name = name; Location = location; Orientation = orientation; }
            in Success (rover)

    ///Access state with f
    let private apply f (Rover state) = f state

    ///Mutate with f
    let private mutate f = apply (fun state -> 
        Rover (f state)
    )

    ///Get the name
    let name = apply (fun state -> state.Name)

    ///True if the name matches
    let called name = apply (fun state ->
        state.Name.Equals (name, StringComparison.OrdinalIgnoreCase)
    )

    ///Get the location
    let location = apply (fun state -> state.Location)

    ///True if the location matches
    let at location = apply (fun state ->
        (Location.toTuple state.Location) = (Location.toTuple location)
    )

    ///True if the same rover
    let equals rover = apply (fun state -> 
        rover |> called state.Name
    )

    ///Move the rover forward
    let move = mutate (fun state ->
        let location = (Location.move (Orientation.direction state.Orientation) state.Location)
        in { state with Location = location; }
    )

    ///Turn the rover using f
    let private turn f = mutate (fun state ->
        let orientation = (f state.Orientation)
        in { state with Orientation = orientation; }
    )

    ///Turn the rover left
    let left = turn Orientation.left

    ///Turn the rover right
    let right = turn Orientation.right

    ///Convert to a string representation
    let toString = apply (fun state ->
        
        let location = Location.toString state.Location
        let orientation = Orientation.toString state.Orientation

        sprintf "%s | %s | Facing %s" state.Name location orientation
    )

[<RequireQualifiedAccess>]
module Planet = 

    type State = {
        Width : int;
        Height : int;
        Rovers : Rover.Instance list;
    }

    type Instance = Planet of State

    ///Create a new planet
    let create width height = 
        if (width <= 0) then
            Problem "The planet's width must be at least 1."
        else if (height <= 0) then
            Problem "The planet's height must be at least 1."
        else
            let planet = Planet { Width = width; Height = height; Rovers = []; }
            in Success (planet)

    ///Access state using f
    let private apply f (Planet state) = f state

    ///Convert to a string representation
    let toString = apply (fun state ->
        let rovers = 
            state.Rovers
            |> List.map Rover.toString
            |> List.toArray
        in String.Join (Environment.NewLine, rovers)
    )

    [<RequireQualifiedAccess>]
    module MissionControl = 

        [<RequireQualifiedAccess>]
        module private Rovers = 

            ///Find a rover by name
            let find (state, name) = 

                let rover = 
                    state.Rovers
                    |> List.tryFind (Rover.called name)

                match rover with
                | Some rover' -> Success (state, rover')
                | _ -> Problem (sprintf "Cannot find rover called %s" name)

            ///Separate a rover from the planet's state
            let extract (state, rover) = 

                let others = 
                    state.Rovers
                    |> List.filter (not << (Rover.equals rover))

                let state' = 
                    { state with Rovers = others; }

                return' (state', rover)

            ///Add a rover to the planet's state
            let add (state, rover) = 

                let others =
                    state.Rovers
                    |> List.filter (not << (Rover.equals rover))

                return' ({ state with Rovers = (rover :: others); })

            ///Update a rover using f
            let update f = 
                fun (state, rover) ->
                    return' (state, (f rover))

        [<RequireQualifiedAccess>]
        module private State = 

            ///Ensure a rover is within bounds
            let checkBounds (state, rover) = 

                let problem msg = 
                    let name = Rover.name rover
                    in Problem (sprintf msg name)

                match (Location.toTuple (Rover.location rover)) with
                | (x, _) when (x < 0) -> problem "Rover %s has moved too far West."
                | (x, _) when (x > state.Width) -> problem "Rover %s has moved too far East."
                | (_, y) when (y < 0) -> problem "Rover %s has moved too far South."
                | (_, y) when (y > state.Height) -> problem "Rover %s has moved too far North."
                | _ -> return' (state, rover)

            ///Ensure a rover is not in collission with another
            let checkCollisions (state, rover) = 
                
                let location = Rover.location rover
                
                let other = 
                    state.Rovers
                    |> List.tryFind (Rover.at location)

                match other with
                | Some rover' -> 
                    Problem (
                        sprintf "Rover %s has collided with rover %s at %s" 
                        <| (Rover.name rover) 
                        <| (Rover.name rover') 
                        <| (Location.toString location)
                    )
                | _ -> Success (state, rover)

            ///Convert a state of a planet
            let toPlanet state = 
                return' (Planet state)

        ///Validate a rover and add it to the planet
        let private validatedAdd = 
            State.checkBounds
            --> State.checkCollisions
            --> Rovers.add
            --> State.toPlanet

        ///Touch a rover down on the planet surface
        let touchdown rover (Planet state) = 
            validatedAdd (state, rover)

        ///Send an instruction to the rover
        let instruct name instruction (Planet state) = 
            let f = 
                Rovers.find
                --> Rovers.extract
                --> Rovers.update instruction
                --> validatedAdd
            in f (state, name)