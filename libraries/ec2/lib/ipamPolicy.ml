open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_policy_id : String.t option
  ; ipam_policy_arn : String.t option
  ; ipam_policy_region : String.t option
  ; state : IpamPolicyState.t option
  ; state_message : String.t option
  ; tags : TagList.t
  ; ipam_id : String.t option
  }

let make
    ?owner_id
    ?ipam_policy_id
    ?ipam_policy_arn
    ?ipam_policy_region
    ?state
    ?state_message
    ?(tags = [])
    ?ipam_id
    () =
  { owner_id
  ; ipam_policy_id
  ; ipam_policy_arn
  ; ipam_policy_region
  ; state
  ; state_message
  ; tags
  ; ipam_id
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_policy_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyId" xml) String.parse
    ; ipam_policy_arn =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyArn" xml) String.parse
    ; ipam_policy_region =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyRegion" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamPolicyState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; ipam_id = Aws.Util.option_bind (Aws.Xml.member "ipamId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_id (fun f ->
             Aws.Query.Pair ("IpamId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamPolicyState.to_query f))
       ; Aws.Util.option_map v.ipam_policy_region (fun f ->
             Aws.Query.Pair ("IpamPolicyRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_policy_arn (fun f ->
             Aws.Query.Pair ("IpamPolicyArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_policy_id (fun f ->
             Aws.Query.Pair ("IpamPolicyId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_id (fun f -> "ipamId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", IpamPolicyState.to_json f)
       ; Aws.Util.option_map v.ipam_policy_region (fun f ->
             "ipamPolicyRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_policy_arn (fun f ->
             "ipamPolicyArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_policy_id (fun f -> "ipamPolicyId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_policy_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyId") String.of_json
  ; ipam_policy_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyArn") String.of_json
  ; ipam_policy_region =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyRegion") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") IpamPolicyState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; ipam_id = Aws.Util.option_map (Aws.Json.lookup j "ipamId") String.of_json
  }
