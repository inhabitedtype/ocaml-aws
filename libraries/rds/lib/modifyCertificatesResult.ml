type t = { certificate : Certificate.t option }

let make ?certificate () = { certificate }

let parse xml =
  Some
    { certificate =
        Aws.Util.option_bind (Aws.Xml.member "Certificate" xml) Certificate.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate (fun f ->
             Aws.Query.Pair ("Certificate", Certificate.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate (fun f -> "Certificate", Certificate.to_json f)
       ])

let of_json j =
  { certificate =
      Aws.Util.option_map (Aws.Json.lookup j "Certificate") Certificate.of_json
  }
