type t = { d_b_snapshot : DBSnapshot.t option }

let make ?d_b_snapshot () = { d_b_snapshot }

let parse xml =
  Some
    { d_b_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshot" xml) DBSnapshot.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot (fun f ->
             Aws.Query.Pair ("DBSnapshot", DBSnapshot.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot (fun f -> "DBSnapshot", DBSnapshot.to_json f)
       ])

let of_json j =
  { d_b_snapshot = Aws.Util.option_map (Aws.Json.lookup j "DBSnapshot") DBSnapshot.of_json
  }
