type t =
  { hosted_zone : HostedZone.t
  ; delegation_set : DelegationSet.t option
  ; v_p_cs : VPCs.t
  }

let make ~hosted_zone ?delegation_set ?(v_p_cs = []) () =
  { hosted_zone; delegation_set; v_p_cs }

let parse xml =
  Some
    { hosted_zone =
        Aws.Xml.required
          "HostedZone"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZone" xml) HostedZone.parse)
    ; delegation_set =
        Aws.Util.option_bind (Aws.Xml.member "DelegationSet" xml) DelegationSet.parse
    ; v_p_cs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "VPCs" xml) VPCs.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VPCs.member", VPCs.to_query v.v_p_cs))
       ; Aws.Util.option_map v.delegation_set (fun f ->
             Aws.Query.Pair ("DelegationSet", DelegationSet.to_query f))
       ; Some (Aws.Query.Pair ("HostedZone", HostedZone.to_query v.hosted_zone))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VPCs", VPCs.to_json v.v_p_cs)
       ; Aws.Util.option_map v.delegation_set (fun f ->
             "DelegationSet", DelegationSet.to_json f)
       ; Some ("HostedZone", HostedZone.to_json v.hosted_zone)
       ])

let of_json j =
  { hosted_zone =
      HostedZone.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZone"))
  ; delegation_set =
      Aws.Util.option_map (Aws.Json.lookup j "DelegationSet") DelegationSet.of_json
  ; v_p_cs = VPCs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPCs"))
  }
