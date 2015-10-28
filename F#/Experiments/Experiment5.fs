namespace FN.Experiments
open FN.tools
open FN
    type Experiment5(data:IData,queries:IData,projrange,pseed:Option<int>,qd,?countop)=
      let count = match countop with
                     |Some count -> count
                     |None -> true
      let projseed = match pseed with
                      |Some seed -> seed
                      |None -> (System.Random()).Next()
      let rnd = System.Random(projseed)
      let byProjc = seq{for proj in projrange do    let DFN = FN.DFN(data,proj,projseed,count,qd)
                                                    let cutlist = [1..2..(4*proj)]
                                                    yield DFN.multiQuery(queries.Data,cutlist)
                                                    }

      member this.result = byProjc
