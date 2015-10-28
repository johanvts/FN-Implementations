namespace FN.Experiments
open FN
  type Experiment2(n,d,c,repeat,dseed:Option<int>,pseed:Option<int>) =
      let rnd = System.Random()
      let dataseed = match dseed with
                        |Some seed -> seed
                        |None -> rnd.Next()
      let projseed = match pseed with
                        |Some seed -> seed
                        |None -> rnd.Next()
      //let seed = 1830613178
      //let seed = 1321496580 //Superseed!

      let l =
        let rho = 1.0/(c ** 2.0)
        let floatl = (float n) ** rho + System.Math.Log(float n)
        (int floatl) + 1

      let projections = l*4
        
        
      let myData = FN.tools.datagenerator(n,d,dataseed,true)
      let zq = [for i in 1..d -> 0.000000000000]
      let queries = 1 //Using origo as data is worst case around origo.
      let rnd = System.Random(projseed)
      let top = 2*l + 1

      //Regular l_2 distance function
      let distance x q =
        let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
        System.Math.Sqrt d2

      member this.result =
              let header = sprintf "Experiment 2 - Samples looked at:\n"
              let seeds = sprintf "Database seed: %d\nProjection seed: %d \n" dataseed projseed
              let repeats = sprintf "Running %d experiments on worst case data.\n" repeat
              let printseq l =
                l|> Seq.take 2 |>Seq.fold (fun s e -> s + (sprintf "%f, " e) )""
                
              let result = seq{for p in 1..repeat do
                                   let F = FN.DFN(myData,projections,c,rnd.Next())
                                   yield match F.samples(zq, 1.2) with
                                           |Some samples -> samples
                                           |None -> -1}
              header + seeds + sprintf "%s" (result|>Seq.fold (fun s x -> s+(string x)+"\n") "")
