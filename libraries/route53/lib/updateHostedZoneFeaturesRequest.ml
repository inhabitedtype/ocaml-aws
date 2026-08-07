open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; enable_accelerated_recovery : Boolean.t option
  }

let make ~hosted_zone_id ?enable_accelerated_recovery () =
  { hosted_zone_id; enable_accelerated_recovery }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; enable_accelerated_recovery =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableAcceleratedRecovery" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_accelerated_recovery (fun f ->
             Aws.Query.Pair ("EnableAcceleratedRecovery", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_accelerated_recovery (fun f ->
             "EnableAcceleratedRecovery", Boolean.to_json f)
       ; Some ("Id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; enable_accelerated_recovery =
      Aws.Util.option_map (Aws.Json.lookup j "EnableAcceleratedRecovery") Boolean.of_json
  }
