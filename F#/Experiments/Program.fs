namespace FN.Experiments
open FN.tools
open Arguments
open System
open System.Text.RegularExpressions

module Program =
      let Run args =
        // Define what arguments are expected
        let defs = [
            {ArgInfo.Command="expno"; Description="Run experiment no(3 or 4)"; Required=true };
            {ArgInfo.Command="data"; Description="Path to dataset"; Required=true };
            {ArgInfo.Command="proj"; Description="Number of projections to use"; Required=false };
            {ArgInfo.Command="q"; Description="Path to queries"; Required=true };
            {ArgInfo.Command="count"; Description="Use sort by count?(default to true)"; Required=false };
            {ArgInfo.Command="qd"; Description="Query dependent?(default to true)"; Required=false };
            {ArgInfo.Command="cuts"; Description="Specify cuts like [start..step..end]"; Required=false };
            {ArgInfo.Command="pseed"; Description="Seed for generating projections. Defaults to random number."; Required=false }]

        let tryRegex pattern input =
          let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
          if m.Success then [m.Groups.[1].Value;m.Groups.[2].Value;m.Groups.[3].Value] |> Some
          else None

        // Parse Arguments into a Dictionary
        let parsedArgs = ParseArgs args defs
        let expno = (int parsedArgs.["expno"])
        let datapath = (string parsedArgs.["data"])
        let qpath = (string parsedArgs.["q"])
        let cuts =(string parsedArgs.["cuts"])

        let cutlist = match (cuts |> tryRegex ".([0-9]*)..([0-9]*)..([0-9]*)]") with
                        |Some list -> let l = List.map (int) list in [(int l.[0])..(int l.[1])..(int l.[2])]
                        |None -> [1..1..1]

        let toInt = function
          | false,_ -> (System.Random()).Next()
          | true, result -> int result

        let toSomeint = function
          | (false, _ ) -> None
          | (true, result) -> Some(int result)


        let toBool = function
          | (false, _ ) -> true
          | (true, result) -> match result with
                               |"True" -> true
                               |"true" -> true
                               |"false" -> false
                               |"False" -> false
                               |_ -> raise(System.Exception("Could not interpret boolean parameter: count"))
          
        let pseed = parsedArgs.TryGetValue("pseed") |> toInt
        let count = parsedArgs.TryGetValue("count") |> toBool
        let projections = parsedArgs.TryGetValue("proj") |> toInt
        let qd = parsedArgs.TryGetValue("qd") |> toBool

        // Parse the data
        let data = FN.tools.DataParser(datapath) :> IData
        let n = data.n
        let d = data.d        

        // Parse the queries
        let queries = FN.tools.DataParser(qpath) :> IData
        let qn = queries.n
        let qdim = queries.d
        
        
        let runExperiment expno =  match qdim = d with
                                    | false -> raise(System.Exception("Queries given for different d than dataset"))
                                    | true -> 
                                         match expno with
                                            |3 ->
                                               let rnd = System.Random(pseed)
                                               let DFN = FN.DFN(data,projections,rnd.Next(),count,qd)
                                               let toAvg (byCut:seq<'a * 'b * float * 'd>)  =
                                                 let cavg = byCut |> Seq.map (fun (a,b,c,d) -> c) |> Seq.average
                                                 let (a,b,_,_) = byCut |> Seq.head
                                                 (a,b,cavg)
                                               let byCut = DFN.multiQuery(queries.Data,cutlist)
                                               for result in byCut do
                                                 let r = toAvg result
                                                 let (proj,cut,avg) = r
                                                 printf "%i %i %f\n" proj cut avg                                                                  
                                            |5 ->
                                                let exp = FN.Experiments.Experiment5(data,queries,[1..2..30],Some(pseed),qd,count)
                                                let byProj = exp.result
                                                for byCut in byProj do
                                                      for success in byCut do
                                                        for (proj,cut,c,recall) in success do
                                                          printf "%i %i %f %s \n" proj cut c (if recall = true then "1.0" else "0.0")
                                            |a -> printf "Experiment no.%d is not defined." a
        runExperiment expno

        

      [<EntryPoint>]  
      let Main(args) =
        Run args
        // main entry point return
        0
