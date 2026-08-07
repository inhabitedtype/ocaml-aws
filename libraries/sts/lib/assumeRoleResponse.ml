open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; assumed_role_user : AssumedRoleUser.t option
  ; packed_policy_size : Integer.t option
  ; source_identity : String.t option
  }

let make ?credentials ?assumed_role_user ?packed_policy_size ?source_identity () =
  { credentials; assumed_role_user; packed_policy_size; source_identity }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; assumed_role_user =
        Aws.Util.option_bind (Aws.Xml.member "AssumedRoleUser" xml) AssumedRoleUser.parse
    ; packed_policy_size =
        Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
    ; source_identity =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             Aws.Query.Pair ("SourceIdentity", String.to_query f))
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
       ; Aws.Util.option_map v.assumed_role_user (fun f ->
             Aws.Query.Pair ("AssumedRoleUser", AssumedRoleUser.to_query f))
       ; Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             "SourceIdentity", String.to_json f)
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             "PackedPolicySize", Integer.to_json f)
       ; Aws.Util.option_map v.assumed_role_user (fun f ->
             "AssumedRoleUser", AssumedRoleUser.to_json f)
       ; Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  ; assumed_role_user =
      Aws.Util.option_map (Aws.Json.lookup j "AssumedRoleUser") AssumedRoleUser.of_json
  ; packed_policy_size =
      Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
  ; source_identity =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentity") String.of_json
  }
