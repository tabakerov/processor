type CPU = {IP: int; A: int; B: int; CRJ: int }

let rawram = [|1; 2; 2; 3; 4|]
let ram = rawram

let ``process`` cpu (ram : int[])  =
    let instruction = ram.[cpu.IP]
    match instruction with
    | 0 -> // noop
        { cpu with IP = cpu.IP + 1; }
    | 1 -> // load next memory item to A
        { cpu with IP = cpu.IP + 2; A = ram.[cpu.IP+1] }
    | 2 -> // add next memory item to A
        { cpu with IP = cpu.IP + 2; A = cpu.A + ram.[cpu.IP+1] }
    | 3 -> // check if A > B and write CJR to IP if true
        match cpu.A > cpu.B with
        | true -> { cpu with IP = cpu.CRJ }
        | false -> { cpu with IP = cpu.IP + 1 }
    | 4 -> // check if A = B and write CJR to IP if true
        match cpu.A = cpu.B with
        | true -> { cpu with IP = cpu.CRJ }
        | false -> { cpu with IP = cpu.IP + 1 }
    | _ -> cpu
        
let cpu = { IP = 0; A = 0; B = 0; }
let n1 = ``process`` cpu rawram
let n2 = ``process`` n1 rawram
let n3 = ``process`` n2 rawram