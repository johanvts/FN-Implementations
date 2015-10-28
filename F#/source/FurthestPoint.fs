namespace FN
open FN.tools

module Seq =
    /// Returns the first n elements from a sequence. 
    //  If there is less the n elements in the sequence the whole sequence is returned.
    let takeSafe n s =
        if s |> Seq.length <= n then s else s|> Seq.take n

type projection = double
type point = double list


type DFN(data:IData,l,seed,?countOp,?queryDependent,?useDepth) =
  let n = data.n
  let d = data.d
  let S = data.Data

  let countFlag = match countOp with
                    |Some count -> count
                    |None -> true

  let qdFlag = match queryDependent with
                 |Some qd -> qd
                 |None -> true

  let depthFlag = match useDepth with
                    |Some rank -> rank
                    |None -> false
 
  ///Perform l random projections of a dataset and store in an l x n matrix
  let projectAsync seed dataset =
    let rnd = System.Random(seed)
    let R = seq{for i in 1..l -> rnd.Next()}
    let projectAsync pseed =
        async{
        let projector = Projector(pseed,d)
        return [for x in dataset -> (projector.project x,x)]        
        }
    R |> Seq.map projectAsync |> Async.Parallel |> Async.RunSynchronously |> Array.toSeq
    
  let project seed dataset =
    let rnd = System.Random(seed)
    let R = seq{for i in 1..l -> rnd.Next()}
    let project pseed =
        let projector = Projector(pseed,d)
        [for x in dataset -> (projector.project x,x)]        
    R |> Seq.map project 

  let rankRows dataset =
    dataset |> Seq.map (Seq.mapi (fun i (proj,x) -> (i,x)))
    
  let depth rank =
      min (rank) (abs (rank - (n+1)))

  ///Sort a dataset by local depth
  ///Each row comes out sorted from shallowest to deepest
  let byDepth projectionset =
    projectionset |> rankRows
    |> Seq.map(Seq.sortBy (fun (rank,x) -> depth rank))
    |> Seq.map(Seq.map(fun (rank,x) -> (double (depth rank),x)))

  ///Sort a dataset by projections
  ///Each row comes out sorted from largest to smallest projections value
  ///The value is inverted so a small value correspond to a large projections. This is to make the logic the same as for depth 
  let byProjection projectionset=
    projectionset |> Seq.map(Seq.sortBy (fun (proj,x) -> - proj)) |> Seq.map(Seq.map(fun (proj,x) -> (-proj,x)))

  //Fast method for filtering sorted tuples based to remove duplicates on second value(in this case the points)
  let distinct (items:seq<('a * 'b)>) =
    seq {
      use e = items.GetEnumerator()
      if e.MoveNext() then
          let prev = ref e.Current
          yield !prev
          while e.MoveNext() do
            if (snd e.Current) <> (snd !prev) then 
              yield e.Current
              prev := e.Current
  }

  //As above but adds a counter 
  let distinctWcount (items:seq<('a * 'b)>) =
    seq {
      use e = items.GetEnumerator()
      let c = ref 1
      let f = e.MoveNext() |> ignore
      let prev = ref e.Current
      while e.MoveNext() do
          match (snd e.Current) = (snd !prev) with
            |true -> 
                  c := (!c + 1)
            |false ->
                 yield (!prev,!c)
                 c := 1
                 prev := e.Current
      yield(!prev,!c)
  }
  
   let noCount pl = pl |> distinct
                       |> Seq.sortBy (fun (param,x) -> param)

   let byCount pl =
     pl |> distinctWcount
                       |> Seq.sortBy (fun ((param,x),dubc) -> param,-dubc )
                       |> Seq.map fst
    
  //Uses the functions above for returning an ordring of the dataset by lowest obtained depth of a point over l random projections of the dataset.
  let preprocess S = 
    S|>projectAsync seed  //Project the dataset. 
    |> if depthFlag then byDepth else byProjection //Sort the database so each row comes out either shallow to deep or large to small projection
    |> Seq.map(Seq.takeSafe (4*l))  //From each row take the first 4l elements

  let buildPermutation byParam =
    byParam |> Seq.concat //Concatenate into a sequence of length l*4l
    |> Seq.sortBy (fun (param,x) -> x) //Sort by point
    |> if countFlag then byCount else noCount //The count flag determines how duplicates are removed. The points are returned in order of param (smallest to largest)
    |> Seq.map snd //Return points
    
  let queryDepStructure = Seq.cache (preprocess S)
  let queryInDepStructure = Seq.cache (preprocess S |> buildPermutation)

  ///Query dependent query
  member this.queryDep((q:float list), top) =
     let projectq = [q] |> project seed |>Seq.map (Seq.head) |> Seq.map fst
     let queryMatrix = Seq.zip projectq queryDepStructure
     let compareByDist (q_r, r_l) =
        let proj_x = r_l|>Seq.head |> fst |> (*) -1.0
        let dist = abs(q_r - proj_x)
        dist
     let headHeap = MaxLeftistHeap.ofSeqBy compareByDist queryMatrix
     Seq.fold (fun (heap,list) e -> let (q_r, r_l) = heap |> MaxLeftistHeap.findMax
                                    let newList = (r_l |> Seq.head)::list
                                    let newHeap = heap |> MaxLeftistHeap.deleteMaxBy compareByDist
                                                    |> MaxLeftistHeap.insertBy compareByDist (q_r,(r_l |> Seq.skip 1))
                                    (newHeap,newList)) (headHeap,[]) {0..top}
     |> snd |> Seq.map snd |> Seq.maxBy (squareddistance q)


  //Query for Furthest neighbor
  //if no top is given revert to linear scan 
  member this.query (q,?cut) =
    match cut with
      |None -> S |> Seq.maxBy (squareddistance q)
      |Some cut ->
        match qdFlag with
          | false -> queryInDepStructure |> Seq.takeSafe cut |> Seq.maxBy (squareddistance q)
          | true -> this.queryDep(q,cut)

  ///Query the structure for multiple queries and cuts parallel along the cuts.
  member this.multiQuery(qlist,cutlist) =
      let byQuery cut = [for q in qlist do
                                         let brute = this.query(q)
                                         let found = this.query(q,cut)
                                         yield ((distance brute q)/(distance found q),brute = found)]
      seq{for cut in cutlist -> async{return [for result in (byQuery cut) -> (l,cut, fst result,snd result)]}}
                  |>Async.Parallel
                  |>Async.RunSynchronously

  member this.seed =
    seed
    

  
  
