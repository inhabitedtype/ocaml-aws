type t = { cpu : CpuPerformanceFactorRequest.t option }

let make ?cpu () = { cpu }

let parse xml =
  Some
    { cpu =
        Aws.Util.option_bind (Aws.Xml.member "Cpu" xml) CpuPerformanceFactorRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu (fun f ->
             Aws.Query.Pair ("Cpu", CpuPerformanceFactorRequest.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu (fun f -> "Cpu", CpuPerformanceFactorRequest.to_json f)
       ])

let of_json j =
  { cpu =
      Aws.Util.option_map (Aws.Json.lookup j "Cpu") CpuPerformanceFactorRequest.of_json
  }
