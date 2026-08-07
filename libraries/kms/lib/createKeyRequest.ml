open Aws.BaseTypes

type t =
  { policy : String.t option
  ; description : String.t option
  ; key_usage : KeyUsageType.t option
  ; customer_master_key_spec : CustomerMasterKeySpec.t option
  ; key_spec : KeySpec.t option
  ; origin : OriginType.t option
  ; custom_key_store_id : String.t option
  ; bypass_policy_lockout_safety_check : Boolean.t option
  ; tags : TagList.t
  ; multi_region : Boolean.t option
  ; xks_key_id : String.t option
  }

let make
    ?policy
    ?description
    ?key_usage
    ?customer_master_key_spec
    ?key_spec
    ?origin
    ?custom_key_store_id
    ?bypass_policy_lockout_safety_check
    ?(tags = [])
    ?multi_region
    ?xks_key_id
    () =
  { policy
  ; description
  ; key_usage
  ; customer_master_key_spec
  ; key_spec
  ; origin
  ; custom_key_store_id
  ; bypass_policy_lockout_safety_check
  ; tags
  ; multi_region
  ; xks_key_id
  }

let parse xml =
  Some
    { policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; key_usage = Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
    ; customer_master_key_spec =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomerMasterKeySpec" xml)
          CustomerMasterKeySpec.parse
    ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) KeySpec.parse
    ; origin = Aws.Util.option_bind (Aws.Xml.member "Origin" xml) OriginType.parse
    ; custom_key_store_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_bind
          (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
          Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; multi_region = Aws.Util.option_bind (Aws.Xml.member "MultiRegion" xml) Boolean.parse
    ; xks_key_id = Aws.Util.option_bind (Aws.Xml.member "XksKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.xks_key_id (fun f ->
             Aws.Query.Pair ("XksKeyId", String.to_query f))
       ; Aws.Util.option_map v.multi_region (fun f ->
             Aws.Query.Pair ("MultiRegion", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
       ; Aws.Util.option_map v.origin (fun f ->
             Aws.Query.Pair ("Origin", OriginType.to_query f))
       ; Aws.Util.option_map v.key_spec (fun f ->
             Aws.Query.Pair ("KeySpec", KeySpec.to_query f))
       ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
             Aws.Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
       ; Aws.Util.option_map v.key_usage (fun f ->
             Aws.Query.Pair ("KeyUsage", KeyUsageType.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.xks_key_id (fun f -> "XksKeyId", String.to_json f)
       ; Aws.Util.option_map v.multi_region (fun f -> "MultiRegion", Boolean.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
             "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             "CustomKeyStoreId", String.to_json f)
       ; Aws.Util.option_map v.origin (fun f -> "Origin", OriginType.to_json f)
       ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", KeySpec.to_json f)
       ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
             "CustomerMasterKeySpec", CustomerMasterKeySpec.to_json f)
       ; Aws.Util.option_map v.key_usage (fun f -> "KeyUsage", KeyUsageType.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ])

let of_json j =
  { policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; key_usage = Aws.Util.option_map (Aws.Json.lookup j "KeyUsage") KeyUsageType.of_json
  ; customer_master_key_spec =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomerMasterKeySpec")
        CustomerMasterKeySpec.of_json
  ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") KeySpec.of_json
  ; origin = Aws.Util.option_map (Aws.Json.lookup j "Origin") OriginType.of_json
  ; custom_key_store_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
  ; bypass_policy_lockout_safety_check =
      Aws.Util.option_map
        (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
        Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; multi_region = Aws.Util.option_map (Aws.Json.lookup j "MultiRegion") Boolean.of_json
  ; xks_key_id = Aws.Util.option_map (Aws.Json.lookup j "XksKeyId") String.of_json
  }
