type t =
  { entity : Entity.t option
  ; metric_data : MetricData.t
  }

let make ?entity ?(metric_data = []) () = { entity; metric_data }

let parse xml =
  Some
    { entity = Aws.Util.option_bind (Aws.Xml.member "Entity" xml) Entity.parse
    ; metric_data =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "MetricData" xml) MetricData.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MetricData.member", MetricData.to_query v.metric_data))
       ; Aws.Util.option_map v.entity (fun f ->
             Aws.Query.Pair ("Entity", Entity.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MetricData", MetricData.to_json v.metric_data)
       ; Aws.Util.option_map v.entity (fun f -> "Entity", Entity.to_json f)
       ])

let of_json j =
  { entity = Aws.Util.option_map (Aws.Json.lookup j "Entity") Entity.of_json
  ; metric_data =
      MetricData.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricData"))
  }
