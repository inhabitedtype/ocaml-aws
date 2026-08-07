open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; d_n_s_name : String.t
  ; evaluate_target_health : Boolean.t
  }

let make ~hosted_zone_id ~d_n_s_name ~evaluate_target_health () =
  { hosted_zone_id; d_n_s_name; evaluate_target_health }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; d_n_s_name =
        Aws.Xml.required
          "DNSName"
          (Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse)
    ; evaluate_target_health =
        Aws.Xml.required
          "EvaluateTargetHealth"
          (Aws.Util.option_bind (Aws.Xml.member "EvaluateTargetHealth" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("EvaluateTargetHealth", Boolean.to_query v.evaluate_target_health))
       ; Some (Aws.Query.Pair ("DNSName", String.to_query v.d_n_s_name))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EvaluateTargetHealth", Boolean.to_json v.evaluate_target_health)
       ; Some ("DNSName", String.to_json v.d_n_s_name)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; d_n_s_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DNSName"))
  ; evaluate_target_health =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EvaluateTargetHealth"))
  }
