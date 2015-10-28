namespace FN

module List =
    let dot x1 x2 = List.fold2 (fun s0 e1 e2 -> (e1*e2) + s0) 0.0 x1 x2

module tools =
  ///Regular l_2 distance function
  let distance x q =
    let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
    System.Math.Sqrt d2

  let squareddistance x q =
    let d2 = List.fold2 (fun sum x1 q1 -> sum+((x1 - q1)*(x1 - q1))) 0.0 x q
    d2
  
  type Projector(seed,d,?p)=
    let rec boxmuller (rand:System.Random) =
      (* Generate two uniform numbers from -1 to 1 *)
      let x = (rand.NextDouble() * 2.0) - 1.0
      let y = (rand.NextDouble() * 2.0) - 1.0
      let s = x * x + y *y
      match s with
        |a when a > 1.0 -> boxmuller rand
        |_ -> x * sqrt (-2.0 * (log s) / s)

    let r = match p with
            | Some list -> list
            | None ->  [for i in 1..d do yield boxmuller (new System.Random(seed+i))]
    
    member t.project x1 =
      List.dot r x1

    member t.pv = r

    member this.seed = seed

    type IData =
      abstract Data : float list list
      abstract n : int
      abstract d : int

    type datagenerator(n,d,seed,compress)=
      let l2norm p = p |> List.sumBy (fun x -> x*x) |> System.Math.Sqrt
      interface IData with
        member this.Data =
          let rnd = new System.Random(seed);
          let genpoint a = [for i in 1..d -> (rnd.NextDouble()
                                            * (if rnd.NextDouble() >0.5 then -1.0 else 1.0)
                                            * (float (rnd.Next())))]
          let data = [for i in 1..n -> genpoint i]
          match compress with
            |false -> data
            |true ->
              match data with
                |[] -> []
                |h::t -> (List.map (fun i -> 2.0*i/(l2norm h))h)::(t|>List.map (fun x -> List.map (fun i -> i/(l2norm x) )x))

        member this.n = n
        member this.d = d

    type DataParser(path) =
      let lines = System.IO.File.ReadAllLines(path) |> Array.toList
      let meta = (lines.[0]).Split(' ')
      let md = meta.[0] |> int
      let mn = meta.[1] |> int
      let parse l = l |> List.tail |> 
                    List.map (fun (x:string) -> x.Split(' ')|>Array.toList)
                    |> List.map (fun line -> line |> List.filter (fun d -> d <> "") |> List.map (double))
      do match md = ((parse lines).[0]|>List.length) && (mn = ((parse lines)|>List.length)) with
             |true -> ()
             |false -> raise(System.Exception("Malformed dataset!"))

      interface IData with
        member this.n = mn
        member this.d = md
        member this.Data = parse lines

[<CustomComparison; CustomEquality>]
type distPointer = { InvDist: float; Row: int; Depth: int}
                      override x.Equals(yobj) =
                        match yobj with
                          | :? distPointer as y -> y.InvDist=x.InvDist && y.Row = x.Row && y.Depth = x.Depth
                          |_ -> false
                      interface System.IComparable<distPointer> with
                        member this.CompareTo {InvDist = dist; Row= _ ; Depth = _ } = compare dist this.InvDist
                      interface System.IComparable with
                        member this.CompareTo yobj =
                          match yobj with
                            | :? distPointer as y -> compare y.InvDist this.InvDist 
                            |_ -> invalidArg "yobj" "cannot compare values of different types"
      
module LeftistHeap =
  type Heap<'Elem when 'Elem:comparison> = E | T of int * 'Elem * Heap<'Elem> * Heap<'Elem>

  let rank = function
    | E -> 0
    | T(r,_,_,_) -> r

  let makeT (x,a,b) =
    if rank a >= rank b
    then T(rank b + 1,x,a,b) //The rank is the number of nodes in the right spine (including the head)
    else T(rank a + 1,x,b,a) 

  let empty = function
    | E -> true
    | _ -> false

  let rec merge = function
    | (h,E) -> h
    | (E,h) -> h
    | (T(_,x,a1,b1) as h1),(T(_,y,a2,b2) as h2) ->
      if x <= y
      then makeT (x,a1,(merge (b1,h2)))
      else makeT (y,a2,(merge (h1,b2)))

  let insert x h =
    merge (h,T(1,x,E,E))

  let deleteMin = function
    | E -> raise(System.Exception("Heap is empty"))
    | T(_,x,a,b) -> merge (a,b)

  let findMin = function
    | E -> raise(System.Exception("Heap is empty"))
    | T(_,x,a,b) -> x

  let ofSeq = function
    | seq -> seq|> Seq.fold (fun s e -> insert e s) E

module MaxLeftistHeap =
  type MaxHeap<'a> = E | T of int * 'a * MaxHeap<'a> * MaxHeap<'a>

  let rank = function
    | E -> 0
    | T(r,_,_,_) -> r

  let makeT (x,a,b) =
    if rank a >= rank b
    then T(rank b + 1,x,a,b) //The rank is the number of nodes in the right spine (including the head)
    else T(rank a + 1,x,b,a) 

  let empty = function
    | E -> true
    | _ -> false

  let rec mergeBy f = function
    | (h,E) -> h
    | (E,h) -> h
    | (T(_,x,a1,b1) as h1),(T(_,y,a2,b2) as h2) ->
      if (f x) >= (f y)  //Reversed for maxheap
      then makeT (x,a1,(mergeBy f (b1,h2)))
      else makeT (y,a2,(mergeBy f (h1,b2)))

  let insertBy f x h =
    mergeBy f (h,T(1,x,E,E))

  let deleteMaxBy f = function
    | E -> raise(System.Exception("Heap is empty"))
    | T(_,x,a,b) -> mergeBy f (a,b)

  let findMax = function
    | E -> raise(System.Exception("Heap is empty"))
    | T(_,x,a,b) -> x

  let ofSeqBy f = function
    | seq -> seq|> Seq.fold (fun s e -> insertBy f e s) E
