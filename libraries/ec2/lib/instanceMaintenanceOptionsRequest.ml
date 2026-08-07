type t = { auto_recovery : InstanceAutoRecoveryState.t option }

let make ?auto_recovery () = { auto_recovery }

let parse xml =
  Some
    { auto_recovery =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoRecovery" xml)
          InstanceAutoRecoveryState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_recovery (fun f ->
             Aws.Query.Pair ("AutoRecovery", InstanceAutoRecoveryState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.auto_recovery (fun f ->
             "AutoRecovery", InstanceAutoRecoveryState.to_json f)
       ])

let of_json j =
  { auto_recovery =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutoRecovery")
        InstanceAutoRecoveryState.of_json
  }
