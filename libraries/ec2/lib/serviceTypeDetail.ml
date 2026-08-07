type t = { service_type : ServiceType.t option }

let make ?service_type () = { service_type }

let parse xml =
  Some
    { service_type =
        Aws.Util.option_bind (Aws.Xml.member "serviceType" xml) ServiceType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_type (fun f ->
             Aws.Query.Pair ("ServiceType", ServiceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_type (fun f ->
             "serviceType", ServiceType.to_json f)
       ])

let of_json j =
  { service_type =
      Aws.Util.option_map (Aws.Json.lookup j "serviceType") ServiceType.of_json
  }
