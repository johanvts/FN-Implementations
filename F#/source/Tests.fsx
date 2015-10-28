#load @"Projector.fs"
#load @"FurthestPoint.fs"

let n = 10
let d = 10
let l = 2

let dataset = FN.tools.datagenerator(n,d,2,true)

let lookatdataset = dataset:>FN.tools.IData
lookatdataset.Data
let qd = true
let project = true
let DFN = FN.DFN(dataset,l,47,false,qd,project)
let size = ((DFN.inspect) |> Seq.length ) = n
let sizeQ = (DFN.inspectQ |> List.map (List.head) |> Seq.length) = l

DFN.inspectQ.[3]
DFN.queryDep([for i in 1..d -> 0.0],l)
DFN.inspect
DFN.CountingTrick
DFN.QueryDependent

