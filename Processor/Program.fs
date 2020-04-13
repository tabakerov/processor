// Learn more about F# at http://fsharp.org

open System

type Memory = int[]

type CPU = {
    IP: int  // instruction pointer
    A: int   // general purpose register
    B: int   // general purpose register
    CJR: int // conditional jump pointer
}

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
