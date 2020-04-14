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
        | Write = 4

        // ARITHMETIC
        | AddAB = 5

        // COMPARISON/CONDITIONS
        | AGreaterB = 6
        | ABEquals = 7
        | SetCJR = 8
        
        | SetJP = 9
        | Jump = 10

    type Ops =
        | Halt
        | Noop

        // MEMORY READ-WRITE
        | LoadA
        | LoadB
        | SwapAB
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
                match instruction with
                | Halt -> // this is the end
                    ``process`` { cpu with Mode = CpuMode.Halt} ram
                | Noop -> // noop
                    ``process`` { cpu with IP = cpu.IP + 1; } ram
                | LoadA -> // load next memory item to A, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; A = ram.[cpu.IP+1] } ram
                | LoadB -> // load next memory item to B, IP jumps over
                    ``process`` { cpu with IP = cpu.IP + 2; B = ram.[cpu.IP+1] } ram
                | Write -> // write A to memory with address B, IP + 1
                    ram.[cpu.B] <- cpu.A
                    ``process`` {cpu with IP = cpu.IP + 1} ram
                | SwapAB -> // A <-> B, IP + 1
                    ``process`` { cpu with IP = cpu.IP + 1; A = cpu.B; B = cpu.A } ram
                | AddAB -> // A + B, put result to  A, IP + 1
                    ``process`` { cpu with IP = cpu.IP + 2; A = cpu.A + ram.[cpu.IP+1] } ram
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

