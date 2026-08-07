type t = { credentials : Credentials.t option }

let make ?credentials () = { credentials }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  }
