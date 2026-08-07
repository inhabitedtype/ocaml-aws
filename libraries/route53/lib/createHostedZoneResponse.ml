open Aws.BaseTypes

type t =
  { hosted_zone : HostedZone.t
  ; change_info : ChangeInfo.t
  ; delegation_set : DelegationSet.t
  ; v_p_c : VPC.t option
  ; location : String.t
  }

let make ~hosted_zone ~change_info ~delegation_set ?v_p_c ~location () =
  { hosted_zone; change_info; delegation_set; v_p_c; location }

let parse xml =
  Some
    { hosted_zone =
        Aws.Xml.required
          "HostedZone"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
    ; change_info =
        Aws.Xml.required
          "ChangeInfo"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
    ; delegation_set =
        Aws.Xml.required
          "DelegationSet"
          (Aws.Util.option_bind (Aws.Xml.member "DelegationSet" xml) DelegationSet.parse)
    ; v_p_c = Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Aws.Util.option_map v.v_p_c (fun f -> Aws.Query.Pair ("VPC", VPC.to_query f))
       ; Some (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
       ; Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info))
       ; Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Aws.Util.option_map v.v_p_c (fun f -> "VPC", VPC.to_json f)
       ; Some ("DelegationSet", DelegationSet.to_json v.delegation_set)
       ; Some ("ChangeInfo", ChangeInfo.to_json v.change_info)
       ; Some ("HostedZone", HostedZone.to_json v.hosted_zone)
       ])

let of_json j =
  { hosted_zone =
      HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
  ; change_info =
      ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
  ; delegation_set =
      DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
  ; v_p_c = Aws.Util.option_map (Aws.Json.lookup j "VPC") VPC.of_json
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
