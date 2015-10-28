namespace FN
open FN.tools

type projection = float
type point = float list


type DFN(data:IData,l,seed) =
  let n = data.n
  let d = data.d
  let S = data.Data
 
  //Perform l random projections of the entire dataset and store in an l x n matrix
  let project seed dataset =
    let rnd = System.Random(seed)
    let R = [for i in 1..l -> Projector(rnd.Next(),d)]
    let projected = [for r_i in R do
                     yield([for x in dataset -> (r_i.project x, x)]|>List.sortBy fst)] //Perform and store l projections of each datapoint
    projected

  //Convert a matrix from projections to local ranks
  let byRank projectionset =
    projectionset |> List.map (fun L -> (L|> List.mapi (fun i (ar,x) -> (i,x))))

  let depth rank =
    min (rank) (abs (rank - (n+1)))

  let invDepth rank =
    (n/2) - (depth rank) //Used because min depth = max  invDepth

  //Regular l_2 distance function
  let distance x q =
    let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
    System.Math.Sqrt d2

  //Functions for finding the lowest obtained depth of a point in a list of (rank,point) tuples.
  let minDepth ranklist (_,point) =
    ranklist 
    |> List.filter (fun (_,x) -> x = point) //Remove other points from the matrix
    |> List.maxBy (fun (rank,_) -> invDepth rank) //Find the lowest achieved depth of any projection
    |> fst |> depth //Return the depth

  //Fast method for filtering sorted tuples based to remove duplicates on second value(in this case the points)
  let distinctWithoutHash (items:seq<('a * 'b)>) =
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

    
  //Uses the functions above for returning an ordring of the dataset by lowest obtained depth of a point over l random projections of the dataset.
  member this.byDepth  =
    let rankList = S|>project seed |> byRank |> List.concat //Project the points, find ranks and concatenate to a list of (rank,point) tuples.
    rankList |> List.sortBy (fun (rank,x) -> x,depth rank) |> distinctWithoutHash
    |> Seq.sortBy (fun (rank,x) -> depth rank) //Sort by depth
    |> Seq.map snd //Return only the points (We do not need the depth going forward)
    


  //Query for Furthest neighbor
  //if no top is given revert to linear scan 
  member this.query (q,?top) =
    match top with
      |Some top -> this.byDepth |> Seq.take top |> Seq.maxBy (distance q)
      |None -> S |> Seq.maxBy (distance q)

  //This method will return the number of samples taken before finding a distance greater than the provided parameter
  member this.samples (q, stopCriteria) =
    let i = this.byDepth |> Seq.tryFindIndex (fun x -> (distance q x) >= stopCriteria)
    i

  //The below methods are exposed for testing purposes but should otherwise not be used.

  member this.proj =
    project seed S

  member this.rank P =
    byRank P

  member this.sortdepth ranked =
    let rankList = ranked |> List.concat
    rankList |> List.sortBy (fun (rank,x) -> x,depth rank) |> distinctWithoutHash
    |> Seq.sortBy (fun (rank,x) -> depth rank) //Sort by depth

  member this.seed =
    seed

  override this.ToString() =
    sprintf "My seed is %d" seed
    

  
  
