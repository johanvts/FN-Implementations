namespace FN

open Utilities.extensions

module tools =
  type Projector(seed,d)=
    let rec boxmuller (rand:System.Random) =
      (* Generate two uniform numbers from -1 to 1 *)
      let x = (rand.NextDouble() * 2.0) - 1.0
      let y = (rand.NextDouble() * 2.0) - 1.0
      let s = x * x + y *y
      match s with
        |a when a > 1.0 -> boxmuller rand
        |_ -> x * sqrt (-2.0 * (log s) / s)

    let r = [for i in 1..d do yield boxmuller (new System.Random(seed+i))]
    
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
                    |> List.map (fun line -> line |> List.filter (fun d -> d <> "") |> List.map (float))
      do match md = ((parse lines).[0]|>List.length) && (mn = ((parse lines)|>List.length)) with
             |true -> ()
             |false -> raise(System.Exception("Malformed dataset!"))

      interface IData with
        member this.n = mn
        member this.d = md
        member this.Data = parse lines
