open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; next_token : String.t option
  ; v_p_cs : VPCs.t
  }

let make ~hosted_zone_id ?next_token ~v_p_cs () = { hosted_zone_id; next_token; v_p_cs }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "HostedZoneId"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; v_p_cs =
        Aws.Xml.required
          "VPCs"
          (Aws.Util.option_bind (Aws.Xml.member "VPCs" xml) VPCs.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VPCs.member", VPCs.to_query v.v_p_cs))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("HostedZoneId", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VPCs", VPCs.to_json v.v_p_cs)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("HostedZoneId", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZoneId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; v_p_cs = VPCs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPCs"))
  }
