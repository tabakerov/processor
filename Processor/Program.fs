// Learn more about F# at http://fsharp.org

open System

type Memory = int[]

type CPU = {
    IP: int
    A: int
    B: int
}

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
