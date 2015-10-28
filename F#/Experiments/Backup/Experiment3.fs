namespace FN.Experiments
open FN.tools
open FN
    type Experiment3(data:IData,queries:IData,projections,pseed:Option<int>,cutrange)=
      let projseed = match pseed with
                      |Some seed -> seed
                      |None -> (System.Random()).Next()
      let rnd = System.Random(projseed)
      let DFN = FN.DFN(data,projections,rnd.Next())
      let distance x q =
        let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
        System.Math.Sqrt d2

      let byQuery cut = [for q in queries.Data do
                                           let brute = DFN.query(q)
                                           let found = DFN.query(q,cut)
                                           yield (distance brute q)/(distance found q)]
      let byCut = [for cut in cutrange -> async{return byQuery cut}]|>Async.Parallel
                  |>Async.RunSynchronously|>Array.toList

      member this.result = byCut
