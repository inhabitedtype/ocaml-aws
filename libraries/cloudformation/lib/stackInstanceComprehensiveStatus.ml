type t = { detailed_status : StackInstanceDetailedStatus.t option }

let make ?detailed_status () = { detailed_status }

let parse xml =
  Some
    { detailed_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DetailedStatus" xml)
          StackInstanceDetailedStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.detailed_status (fun f ->
             Aws.Query.Pair ("DetailedStatus", StackInstanceDetailedStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.detailed_status (fun f ->
             "DetailedStatus", StackInstanceDetailedStatus.to_json f)
       ])

let of_json j =
  { detailed_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "DetailedStatus")
        StackInstanceDetailedStatus.of_json
  }
