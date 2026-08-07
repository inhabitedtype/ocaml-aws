type t = { conversion_task : ConversionTask.t option }

let make ?conversion_task () = { conversion_task }

let parse xml =
  Some
    { conversion_task =
        Aws.Util.option_bind (Aws.Xml.member "conversionTask" xml) ConversionTask.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.conversion_task (fun f ->
             Aws.Query.Pair ("ConversionTask", ConversionTask.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.conversion_task (fun f ->
             "conversionTask", ConversionTask.to_json f)
       ])

let of_json j =
  { conversion_task =
      Aws.Util.option_map (Aws.Json.lookup j "conversionTask") ConversionTask.of_json
  }
