type t = { reserved_d_b_instance : ReservedDBInstance.t option }

let make ?reserved_d_b_instance () = { reserved_d_b_instance }

let parse xml =
  Some
    { reserved_d_b_instance =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedDBInstance" xml)
          ReservedDBInstance.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_d_b_instance (fun f ->
             Aws.Query.Pair ("ReservedDBInstance", ReservedDBInstance.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_d_b_instance (fun f ->
             "ReservedDBInstance", ReservedDBInstance.to_json f)
       ])

let of_json j =
  { reserved_d_b_instance =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedDBInstance")
        ReservedDBInstance.of_json
  }
