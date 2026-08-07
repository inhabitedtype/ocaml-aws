open Aws.BaseTypes

type t =
  { key_id : String.t
  ; replica_region : String.t
  ; policy : String.t option
  ; bypass_policy_lockout_safety_check : Boolean.t option
  ; description : String.t option
  ; tags : TagList.t
  }

let make
    ~key_id
    ~replica_region
    ?policy
    ?bypass_policy_lockout_safety_check
    ?description
    ?(tags = [])
    () =
  { key_id
  ; replica_region
  ; policy
  ; bypass_policy_lockout_safety_check
  ; description
  ; tags
  }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; replica_region =
        Aws.Xml.required
          "ReplicaRegion"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicaRegion" xml) String.parse)
    ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_bind
          (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
          Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ; Some (Aws.Query.Pair ("ReplicaRegion", String.to_query v.replica_region))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ; Some ("ReplicaRegion", String.to_json v.replica_region)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; replica_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaRegion"))
  ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; bypass_policy_lockout_safety_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
        Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
