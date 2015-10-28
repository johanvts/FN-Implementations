namespace FN.Experiments
open FN.tools
open Arguments
open System
open System.Text.RegularExpressions

module Program =
      let Run args =
        // Define what arguments are expected
        let defs = [
            {ArgInfo.Command="data"; Description="Path to dataset"; Required=true };
            {ArgInfo.Command="projections"; Description="Number of projections to use"; Required=true };
            {ArgInfo.Command="q"; Description="Path to queries"; Required=false };
            {ArgInfo.Command="cuts"; Description="Specify cuts like [start..step..end]"; Required=true };
            {ArgInfo.Command="pseed"; Description="Seed for generating projections. Defaults to random number."; Required=false }]

        let tryRegex pattern input =
          let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
          if m.Success then [m.Groups.[1].Value;m.Groups.[2].Value;m.Groups.[3].Value] |> Some
          else None

        // Parse Arguments into a Dictionary
        let parsedArgs = ParseArgs args defs
        let datapath = (string parsedArgs.["data"])
        let projections = (int parsedArgs.["projections"])
        let qpath = (string parsedArgs.["q"])
        let cuts =(string parsedArgs.["cuts"])

        let cutl = match (cuts |> tryRegex ".([0-9]*)..([0-9]*)..([0-9]*)]") with
                    |Some list -> let l = List.map (int) list in [(int l.[0])..(int l.[1])..(int l.[2])]
                    |None -> [1..1..1]
  

        let toSomeint = function
          | (false, _ ) -> None
          | (true, result) -> Some(int result)
          
        let pseed = parsedArgs.TryGetValue("pseed") |> toSomeint

        // Parse the data
        let data = FN.tools.DataParser(datapath) :> IData
        let n = data.n
        let d = data.d        

        // Parse the queries
        let queries = FN.tools.DataParser(qpath) :> IData
        let qn = queries.n
        let qd = queries.d 
        
        let exp3 =  match qd = d with
                       | false -> raise(System.Exception("Queries given for difference d than dataset"))
                       | true -> FN.Experiments.Experiment3(data,queries,projections,pseed,cutl)
                       
        let success = exp3.result
        let printlist l =
                l|>List.fold (fun s e -> s + (sprintf "%f, " e) )""
        printf "%s" (List.fold (fun s e -> s + printlist e) "" success)

      [<EntryPoint>]  
      let Main(args) =
        Run args
        // main entry point return
        0
