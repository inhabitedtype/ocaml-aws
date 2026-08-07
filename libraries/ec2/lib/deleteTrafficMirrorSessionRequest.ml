open Aws.BaseTypes

type t =
  { traffic_mirror_session_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~traffic_mirror_session_id ?dry_run () = { traffic_mirror_session_id; dry_run }

let parse xml =
  Some
    { traffic_mirror_session_id =
        Aws.Xml.required
          "TrafficMirrorSessionId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TrafficMirrorSessionId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TrafficMirrorSessionId", String.to_query v.traffic_mirror_session_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TrafficMirrorSessionId", String.to_json v.traffic_mirror_session_id)
       ])

let of_json j =
  { traffic_mirror_session_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrafficMirrorSessionId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
