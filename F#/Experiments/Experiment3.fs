namespace FN.Experiments
open FN.tools
open FN
module Experiments = 
    let Experiment3 (data:IData) (queries:IData) projections pseed cutlist countop qdop =
      let count = match countop with
                     |Some count -> count
                     |None -> true

      let qd = match qdop with
                  | Some bool -> bool
                  | None -> true

      let projseed = match pseed with
                      |Some seed -> seed
                      |None -> (System.Random()).Next()
      let rnd = System.Random(projseed)
      let DFN = FN.DFN(data,projections,rnd.Next(),count,qd)
      DFN.multiQuery(queries.Data,cutlist)
