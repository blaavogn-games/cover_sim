//Simulation vars
let persons = 10
let weapons = 9
let rounds = 5 
let players = 3 //Cooperating players
let simulations = 100000
let singleWeaponDraw = 2
let singlePersonDraw = 3

type info =
    | S of int
    | D of int * int

//Util functions
let rand = new System.Random ((int) (System.Diagnostics.Stopwatch.GetTimestamp ()))

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

let shuffle a =
    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

//Game
let generateDeck n m ci singles =
    let ciCount = Array.zeroCreate ci
    seq {
            for i in 1 .. m do //id 0 is always murder/murderWeapon
                for j in i + 1 .. m do
                    if (i <= ci && ciCount.[i - 1] > 3) || (j <= ci && ciCount.[j - 1] > 3) then 
                        ()
                    else
                        yield D(i, j)
            for i in n .. m do
                for j in 1 .. singles do
                    yield S(i)
                    yield S(i)
        } |> Seq.toArray
    

let simulateGame _ =
    let personDeck = generateDeck 4 (persons - 1) 3 singlePersonDraw
    let weaponDeck = generateDeck 1 (weapons - 1) 0 singleWeaponDraw
    
    shuffle personDeck
    shuffle weaponDeck
    
    //Drawing cards
    let personDraws = 
        seq {
            for i in 0 .. (players - 1) do
                yield personDeck.[i * 3 .. i * 3 + 3]
        } |> Seq.toArray
    
    let weaponDraws = 
        seq {
            for i in 0 .. (players - 1) do
                yield weaponDeck.[i * 2 .. i * 2 + 2]
        } |> Seq.toArray
    
    //Counting hits
    let personHits = Array.zeroCreate persons
    let weaponHits = Array.zeroCreate weapons
    
    let count (count: int array) arr =
        //printfn "pl"
        let (c: int array) = Array.zeroCreate count.Length
        Array.iter (fun (e) ->
                        match e with
                        | S(e1)    -> c.[e1] <- c.[e1] + 1
                        | D(e1,e2) -> c.[e1] <- c.[e1] + 1; 
                                      c.[e2] <- c.[e2] + 1
                    ) arr 
        Array.iteri (fun i e -> if e > 0 then count.[i] <- count.[i]+ 1) c

    Array.iter (count personHits) personDraws
    //printfn "Weapons"
    Array.iter (count weaponHits) weaponDraws

    let p = Array.fold (fun acc e -> if e < 2 then acc + 1 else acc) 0 personHits
    let w = Array.fold (fun acc e -> if e < 2 then acc + 1 else acc) 0 weaponHits

    (p,w)

[<EntryPoint>]
let main argv =     
    let personsLeft = Array.zeroCreate persons
    let weaponsLeft = Array.zeroCreate persons

    for _ in 1 .. simulations do
        let (p,w) = simulateGame ()
        personsLeft.[p] <- personsLeft.[p] + 1
        weaponsLeft.[w] <- weaponsLeft.[w] + 1

    printfn "No CI-hints, single and double hints"
    printfn "Cooperating players: %d" players
    printfn "Rounds: %d" rounds
    printfn "Weapons: %d" weapons
    printfn "Suspects: %d" persons
    printfn "Single weapons: %d " singleWeaponDraw
    printfn "Single persons: %d " singlePersonDraw
    printfn "Simulations: %d \n" simulations
    
    printfn "  Info |  Suspe | Weapon"
    Array.iteri2 (fun i p w -> printfn "%6.0f | %6.0f | %6.0f" ((float) i) ((float)p) ((float) w)) personsLeft weaponsLeft
    0 
