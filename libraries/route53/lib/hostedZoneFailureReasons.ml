open Aws.BaseTypes

type t = { accelerated_recovery : String.t option }

let make ?accelerated_recovery () = { accelerated_recovery }

let parse xml =
  Some
    { accelerated_recovery =
        Aws.Util.option_bind (Aws.Xml.member "AcceleratedRecovery" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.accelerated_recovery (fun f ->
             Aws.Query.Pair ("AcceleratedRecovery", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.accelerated_recovery (fun f ->
             "AcceleratedRecovery", String.to_json f)
       ])

let of_json j =
  { accelerated_recovery =
      Aws.Util.option_map (Aws.Json.lookup j "AcceleratedRecovery") String.of_json
  }
