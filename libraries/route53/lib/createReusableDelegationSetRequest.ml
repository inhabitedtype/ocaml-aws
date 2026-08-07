open Aws.BaseTypes

type t =
  { caller_reference : String.t
  ; hosted_zone_id : String.t option
  }

let make ~caller_reference ?hosted_zone_id () = { caller_reference; hosted_zone_id }

let parse xml =
  Some
    { caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("HostedZoneId", String.to_query f))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.hosted_zone_id (fun f -> "HostedZoneId", String.to_json f)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ])

let of_json j =
  { caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
  }
