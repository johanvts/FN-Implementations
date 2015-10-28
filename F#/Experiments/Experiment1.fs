namespace FN.Experiments
open FN.tools
open FN
    type Experiment1(n,d,?dseed,?pseed) =
      let rnd = System.Random()
      let dataseed = match dseed with
                        |Some seed -> seed
                        |None -> rnd.Next()
      let projseed = match pseed with
                        |Some seed -> seed
                        |None -> rnd.Next()
      //let seed = 1830613178
      //let seed = 1321496580 //Superseed!

      let l = 30
      let projections = l
        
        
      let myData = datagenerator(n,d,dataseed,true)
      let zq = [for i in 1..d -> 0.000000000000]
      let queries = 1 //Using origo as data is worst case around origo.
      let repeat = 1
      let rnd = System.Random(projseed)
      let top = 2*l + 1

      //Regular l_2 distance function
      let distance x q =
        let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
        System.Math.Sqrt d2
      
      member this.result =
              let header = sprintf "Experiment 1:\n"
              let seeds  = sprintf "Database seed: %d\nProjection seed: %d \n" dataseed projseed
              let result = seq{for p in 1..repeat ->
                               let F = DFN(myData,projections,56,false,false)
                               [for q in 1..queries -> async{return if (distance zq (F.query (zq,top))) < 1.2 then 0 else 1}]
                               |>Async.Parallel|>Async.RunSynchronously|>Array.sum}
              header+seeds+ (sprintf "Found the correct point %d times.\n" (result |> Seq.sum))
      
