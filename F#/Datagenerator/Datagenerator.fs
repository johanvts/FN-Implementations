namespace FN.Datagenerator
open FN.Arguments
open System

module Program =
  
      type datagenerator(n,d,seed,datatype)=
        let l2norm p = p |> Seq.sumBy (fun x -> x*x) |> System.Math.Sqrt
        let rnd = new System.Random(seed)
        let genpoint a = seq{for i in 1..d ->(rnd.NextDouble()
                                                    * (if rnd.NextDouble() >0.5 then -1.0 else 1.0)
                                                    * (float (rnd.NextDouble())))}

        let rec boxmuller re (rand:System.Random) =
          (* Generate two uniform numbers from -1 to 1 *)
          let x = (rand.NextDouble() * 2.0) - 1.0
          let y = (rand.NextDouble() * 2.0) - 1.0
          let s = x * x + y * y
          match s with
            |a when a > 1.0 -> boxmuller (int a)  rand
            |_ -> printf "%f " (x * sqrt (-2.0 * (log s) / s))

        let printpoint p =
             (p|> Seq.fold (fun s ki -> s + (sprintf "%f " ki)) "")+"\n"

        member this.data = 
          match datatype with
             |1 ->   for i in 1..n do printf "%s" (printpoint(genpoint i))
             |2 ->   for i in 1..n do
                            let p = genpoint i
                            match i with
                               |1 -> printf "%s" (printpoint (Seq.map (fun i -> 2.0*i/(l2norm p)) p))
                               |_ -> printf "%s" (printpoint ( Seq.map (fun i -> i/(l2norm p)) p))
             |3 ->   for o in 1..n do
                            for i in 1..d do boxmuller (i+o) rnd
                            printf "\n"
             |_ -> raise(System.Exception("Invalid datatype selected"))
          
           
         member this.n = n
         member this.d = d
         
      let Run args =
        // Define what arguments are expected
        let defs = [
            {ArgInfo.Command="d"; Description="Dimensionality of dataset"; Required=true };
            {ArgInfo.Command="n"; Description="Number of points in dataset"; Required=true };
            {ArgInfo.Command="dseed"; Description="Seed for generating database. Defaults to random number."; Required=false };
            {ArgInfo.Command="type"; Description="Type = 1: Uniform(default), Type 2 = Ring wiht point at 2x ring radius, Type 3 = Gaussians"; Required=false }
            ]

        // Parse Arguments into a Dictionary
        let parsedArgs = ParseArgs args defs

        let n = (int parsedArgs.["n"])
        let d = (int parsedArgs.["d"])

        let toSomeint = function
          | (false, _ ) -> None
          | (true, result) -> Some(int result)
          
        let dseed = parsedArgs.TryGetValue("dseed") |> toSomeint
        let no = parsedArgs.TryGetValue("type") |> toSomeint
        
        let datatype  = match no with
                         |Some number -> number
                         |None -> 1

        let seed = match dseed with
                       |Some number -> number
                       |None -> (new System.Random()).Next()
        
        let data = datagenerator(n,d,seed,datatype)
        data

         
          

      [<EntryPoint>]  
      let Main(args) =
        let dataset = Run args
        printfn "%d %d 2" dataset.d dataset.n
        dataset.data
        0
