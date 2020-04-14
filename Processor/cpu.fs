module CPU

    open System

    type CpuMode =
    | Run
    | Halt

    type CPU = {
        Mode: CpuMode
        IP : int    // instruction pointer
        A  : int    // general purpose register
        B  : int    // general purpose register
        RWA: int    // register for memory address to read/write
        JP : int    // unconditional jump pointer
        CJR: int    // conditional jump pointer
    }

    type OpsEnum =
        | Halt = -1
        | Noop = 0

        // MEMORY READ-WRITE
        | LoadA = 1
        | LoadB = 2
        | SwapAB = 3
        | SetRWA = 4
        | Read  = 5
        | Write = 6

        // ARITHMETIC
        | AddAB = 7

        // COMPARISON/CONDITIONS
        | AGreaterB = 8
        | ABEquals = 9
        | SetCJR = 10
        
        | SetJP = 11
        | Jump = 12

    type Ops =
        | Halt
        | Noop

        // MEMORY READ-WRITE
        | LoadA
        | LoadB
        | SwapAB
        | SetRWA
        | Read
        | Write
     
        // ARITHMETIC
        | AddAB

        // COMPARISON/CONDITIONS
        | AGreaterB
        | ABEquals
        | SetCJR
        | SetJP
        | Jump

    let decodeInstruction (i : int) : Ops =
        match enum<OpsEnum>(i) with
        | OpsEnum.Halt -> Halt
        | OpsEnum.Noop -> Noop

        // MEMORY READ-WRITE
        | OpsEnum.LoadA -> LoadA
        | OpsEnum.LoadB -> LoadB
        | OpsEnum.SetRWA -> SetRWA
        | OpsEnum.Read -> Read
        | OpsEnum.Write -> Write
        | OpsEnum.SwapAB -> SwapAB

        // ARITHMETIC
        | OpsEnum.AddAB -> AddAB

        // COMPARISON/CONDITIONS
        | OpsEnum.AGreaterB -> AGreaterB
        | OpsEnum.ABEquals -> ABEquals
        | OpsEnum.SetCJR -> SetCJR
        | OpsEnum.SetJP -> SetJP
        | OpsEnum.Jump -> Jump
        | _ -> Noop

    let encodeInstruction (i : Ops) : int =
        match i with
        | Halt -> int OpsEnum.Halt
        | Noop -> int OpsEnum.Noop
        | LoadA -> int OpsEnum.LoadA
        | LoadB -> int OpsEnum.LoadB
        | SetRWA -> int OpsEnum.SetRWA
        | Read -> int OpsEnum.Read
        | Write -> int OpsEnum.Write
        | SwapAB -> int OpsEnum.SwapAB
        | AddAB -> int OpsEnum.AddAB
        | AGreaterB -> int OpsEnum.AGreaterB
        | ABEquals -> int OpsEnum.ABEquals
        | SetCJR -> int OpsEnum.SetCJR
        | SetJP -> int OpsEnum.SetJP
        | Jump -> int OpsEnum.Jump
    
    let rec ``process`` cpu (ram : int[])  =
        match cpu.IP >= ram.Length with
        | true -> (cpu, ram)
        | false ->
            match cpu.Mode with
            | CpuMode.Halt -> (cpu, ram)
            | CpuMode.Run ->
                let instruction = ram.[cpu.IP] |> decodeInstruction
                // printfn "Instruction: %A, CPU: %A" instruction cpu
                // Console.ReadKey() |> ignore
                match instruction with
                | Halt -> // this is the end
                    ``process`` { cpu with Mode = CpuMode.Halt} ram
                | Noop -> // noop
                    ``process`` { cpu with IP = cpu.IP + 1; } ram
                | LoadA -> // load next memory item to A, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; A = ram.[cpu.IP+1] } ram
                | LoadB -> // load next memory item to B, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; B = ram.[cpu.IP+1] } ram
                | SetRWA -> // load next memory item to RWA, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; RWA = ram.[cpu.IP+1] } ram
                | Read -> // load from RWA memory address to A, IP + 1
                    ``process`` { cpu with IP = cpu.IP + 1; A = ram.[cpu.RWA]} ram
                | Write -> // write A to memory with address RWA, IP + 1
                    ram.[cpu.RWA] <- cpu.A
                    ``process`` {cpu with IP = cpu.IP + 1} ram
                | SwapAB -> // A <-> B, IP + 1
                    ``process`` { cpu with IP = cpu.IP + 1; A = cpu.B; B = cpu.A } ram
                | AddAB -> // A + B, put result to  A, IP + 1
                    ``process`` { cpu with IP = cpu.IP + 1; A = cpu.A + cpu.B } ram
                | AGreaterB -> // check if A > B and write CJR to IP if true
                    match cpu.A > cpu.B with
                    | true -> ``process`` { cpu with IP = cpu.CJR } ram
                    | false -> ``process`` { cpu with IP = cpu.IP + 1 } ram
                | ABEquals -> // check if A = B and write CJR to IP if true
                    match cpu.A = cpu.B with
                    | true -> ``process`` { cpu with IP = cpu.CJR } ram
                    | false -> ``process`` { cpu with IP = cpu.IP + 1 } ram
                | SetCJR -> // set CJR = next memory item, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; CJR = ram.[cpu.IP+1] } ram
                | SetJP -> // set JP = next memory item, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; JP = ram.[cpu.IP+1] } ram
                | Jump -> // set IP = JP
                    ``process`` { cpu with IP = cpu.JP } ram

