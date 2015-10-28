namespace FN.Experiments
open FN.tools
open FN
    type Experiment4(data:IData,queries:IData,projrange,pseed:Option<int>,cutrange:int list,?countop)=
      let distance x q =
        let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
        System.Math.Sqrt d2

      let count = match countop with
                     |Some count -> count
                     |None -> true
      let projseed = match pseed with
                      |Some seed -> seed
                      |None -> (System.Random()).Next()
      let rnd = System.Random(projseed)
      let byProjc = seq{for projc in projrange do
                                             let DFN = FN.DFN(data,projc,rnd.Next(),count)
                                             let byQuery cut = [for q in queries.Data do
                                                                            let brute = DFN.query(q)
                                                                            let found = DFN.query(q,cut) 
                                                                            yield (distance brute q)/(distance found q)] |>List.average
                                             yield [for cut in cutrange -> async{return (projc,cut, (byQuery cut))}]|>Async.Parallel
                                             |>Async.RunSynchronously|>Array.toList}|>List.concat

      member this.result = byProjc
