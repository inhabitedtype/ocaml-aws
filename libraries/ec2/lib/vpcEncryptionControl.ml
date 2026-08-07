open Aws.BaseTypes

type t =
  { vpc_id : String.t option
  ; vpc_encryption_control_id : String.t option
  ; mode : VpcEncryptionControlMode.t option
  ; state : VpcEncryptionControlState.t option
  ; state_message : String.t option
  ; resource_exclusions : VpcEncryptionControlExclusions.t option
  ; tags : TagList.t
  }

let make
    ?vpc_id
    ?vpc_encryption_control_id
    ?mode
    ?state
    ?state_message
    ?resource_exclusions
    ?(tags = [])
    () =
  { vpc_id
  ; vpc_encryption_control_id
  ; mode
  ; state
  ; state_message
  ; resource_exclusions
  ; tags
  }

let parse xml =
  Some
    { vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; vpc_encryption_control_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEncryptionControlId" xml) String.parse
    ; mode =
        Aws.Util.option_bind (Aws.Xml.member "mode" xml) VpcEncryptionControlMode.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) VpcEncryptionControlState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; resource_exclusions =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceExclusions" xml)
          VpcEncryptionControlExclusions.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.resource_exclusions (fun f ->
             Aws.Query.Pair
               ("ResourceExclusions", VpcEncryptionControlExclusions.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpcEncryptionControlState.to_query f))
       ; Aws.Util.option_map v.mode (fun f ->
             Aws.Query.Pair ("Mode", VpcEncryptionControlMode.to_query f))
       ; Aws.Util.option_map v.vpc_encryption_control_id (fun f ->
             Aws.Query.Pair ("VpcEncryptionControlId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.resource_exclusions (fun f ->
             "resourceExclusions", VpcEncryptionControlExclusions.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", VpcEncryptionControlState.to_json f)
       ; Aws.Util.option_map v.mode (fun f -> "mode", VpcEncryptionControlMode.to_json f)
       ; Aws.Util.option_map v.vpc_encryption_control_id (fun f ->
             "vpcEncryptionControlId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ])

let of_json j =
  { vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; vpc_encryption_control_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEncryptionControlId") String.of_json
  ; mode = Aws.Util.option_map (Aws.Json.lookup j "mode") VpcEncryptionControlMode.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") VpcEncryptionControlState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; resource_exclusions =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceExclusions")
        VpcEncryptionControlExclusions.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
