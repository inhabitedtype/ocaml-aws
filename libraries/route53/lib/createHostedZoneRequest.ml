open Aws.BaseTypes

type t =
  { name : String.t
  ; v_p_c : VPC.t option
  ; caller_reference : String.t
  ; hosted_zone_config : HostedZoneConfig.t option
  ; delegation_set_id : String.t option
  }

let make ~name ?v_p_c ~caller_reference ?hosted_zone_config ?delegation_set_id () =
  { name; v_p_c; caller_reference; hosted_zone_config; delegation_set_id }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; v_p_c = Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse
    ; caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; hosted_zone_config =
        Aws.Util.option_bind
          (Aws.Xml.member "HostedZoneConfig" xml)
          HostedZoneConfig.parse
    ; delegation_set_id =
        Aws.Util.option_bind (Aws.Xml.member "DelegationSetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delegation_set_id (fun f ->
             Aws.Query.Pair ("DelegationSetId", String.to_query f))
       ; Aws.Util.option_map v.hosted_zone_config (fun f ->
             Aws.Query.Pair ("HostedZoneConfig", HostedZoneConfig.to_query f))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ; Aws.Util.option_map v.v_p_c (fun f -> Aws.Query.Pair ("VPC", VPC.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delegation_set_id (fun f ->
             "DelegationSetId", String.to_json f)
       ; Aws.Util.option_map v.hosted_zone_config (fun f ->
             "HostedZoneConfig", HostedZoneConfig.to_json f)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ; Aws.Util.option_map v.v_p_c (fun f -> "VPC", VPC.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; v_p_c = Aws.Util.option_map (Aws.Json.lookup j "VPC") VPC.of_json
  ; caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; hosted_zone_config =
      Aws.Util.option_map (Aws.Json.lookup j "HostedZoneConfig") HostedZoneConfig.of_json
  ; delegation_set_id =
      Aws.Util.option_map (Aws.Json.lookup j "DelegationSetId") String.of_json
  }
