// Calculate the biggest Fibonacci number <= input argument 

open System
open CPU

let cpu = { Mode=Run; IP = 0; A = 0; B = 0; CJR = 0; JP = 0; RWA = 0 }
let resultAddress = 100 // result will be written here
let inputAddress = 99 // input argument goes here
let finalSequenceAddress = 80
let tempValueAddress = 98 
let cycleStart = 20

let memory = seq<int>{for i in 0 .. resultAddress -> 0} |> Array.ofSeq

memory.[inputAddress] <- 22 // Limit

// init first two Fibonacci numbers: 1, 1
memory.[0]  <- encodeInstruction LoadA
memory.[1]  <- 1
memory.[2]  <- encodeInstruction LoadB
memory.[3]  <- 1

// set address to jump to if finished
memory.[4]  <- encodeInstruction SetCJR
memory.[5]  <- finalSequenceAddress

memory.[6]  <- encodeInstruction SetJP
memory.[7]  <- cycleStart
memory.[8]  <- encodeInstruction Jump

// calculate next Fibonacci number
memory.[cycleStart + 0]  <- encodeInstruction AddAB
// now there is a current Fn in A, store previous value to result memory address and compare it with limit
memory.[cycleStart + 1]  <- encodeInstruction SwapAB
memory.[cycleStart + 2]  <- encodeInstruction SetRWA
memory.[cycleStart + 3]  <- resultAddress
memory.[cycleStart + 4]  <- encodeInstruction Write

memory.[cycleStart + 5]  <- encodeInstruction SetRWA
memory.[cycleStart + 6]  <- tempValueAddress
memory.[cycleStart + 7]  <- encodeInstruction Write // write F(n-1) to memory

memory.[cycleStart + 8]  <- encodeInstruction SetRWA // reading limit from memory
memory.[cycleStart + 9]  <- inputAddress
memory.[cycleStart + 10]  <- encodeInstruction Read
// now A = limit, B = Fn, need swap
memory.[cycleStart + 11]  <- encodeInstruction SwapAB
memory.[cycleStart + 12]  <- encodeInstruction AGreaterB // if A more than the limit - jump to final sequence
// not greater, need to read F(n-1) from memory
memory.[cycleStart + 13]  <- encodeInstruction SwapAB
memory.[cycleStart + 14] <- encodeInstruction SetRWA
memory.[cycleStart + 15] <- tempValueAddress
memory.[cycleStart + 16] <- encodeInstruction Read // now A = F(n-1) ; B = Fn

// going back to loop start
// memory.[cycleStart + 4] <- encodeInstruction SwapAB
memory.[cycleStart + 17] <- encodeInstruction SetJP
memory.[cycleStart + 18] <- cycleStart
memory.[cycleStart + 19] <- encodeInstruction Jump

// final
memory.[finalSequenceAddress + 0] <- encodeInstruction Halt


[<EntryPoint>]
let main argv =
    printfn "%A" memory
    let (c, m) = ``process`` cpu memory
    printfn "%A" m.[resultAddress]
    0 // return an integer exit code
