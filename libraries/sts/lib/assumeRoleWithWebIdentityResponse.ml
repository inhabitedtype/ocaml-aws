open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; subject_from_web_identity_token : String.t option
  ; assumed_role_user : AssumedRoleUser.t option
  ; packed_policy_size : Integer.t option
  ; provider : String.t option
  ; audience : String.t option
  ; source_identity : String.t option
  }

let make
    ?credentials
    ?subject_from_web_identity_token
    ?assumed_role_user
    ?packed_policy_size
    ?provider
    ?audience
    ?source_identity
    () =
  { credentials
  ; subject_from_web_identity_token
  ; assumed_role_user
  ; packed_policy_size
  ; provider
  ; audience
  ; source_identity
  }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; subject_from_web_identity_token =
        Aws.Util.option_bind
          (Aws.Xml.member "SubjectFromWebIdentityToken" xml)
          String.parse
    ; assumed_role_user =
        Aws.Util.option_bind (Aws.Xml.member "AssumedRoleUser" xml) AssumedRoleUser.parse
    ; packed_policy_size =
        Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
    ; provider = Aws.Util.option_bind (Aws.Xml.member "Provider" xml) String.parse
    ; audience = Aws.Util.option_bind (Aws.Xml.member "Audience" xml) String.parse
    ; source_identity =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             Aws.Query.Pair ("SourceIdentity", String.to_query f))
       ; Aws.Util.option_map v.audience (fun f ->
             Aws.Query.Pair ("Audience", String.to_query f))
       ; Aws.Util.option_map v.provider (fun f ->
             Aws.Query.Pair ("Provider", String.to_query f))
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             Aws.Query.Pair ("PackedPolicySize", Integer.to_query f))
       ; Aws.Util.option_map v.assumed_role_user (fun f ->
             Aws.Query.Pair ("AssumedRoleUser", AssumedRoleUser.to_query f))
       ; Aws.Util.option_map v.subject_from_web_identity_token (fun f ->
             Aws.Query.Pair ("SubjectFromWebIdentityToken", String.to_query f))
       ; Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             "SourceIdentity", String.to_json f)
       ; Aws.Util.option_map v.audience (fun f -> "Audience", String.to_json f)
       ; Aws.Util.option_map v.provider (fun f -> "Provider", String.to_json f)
       ; Aws.Util.option_map v.packed_policy_size (fun f ->
             "PackedPolicySize", Integer.to_json f)
       ; Aws.Util.option_map v.assumed_role_user (fun f ->
             "AssumedRoleUser", AssumedRoleUser.to_json f)
       ; Aws.Util.option_map v.subject_from_web_identity_token (fun f ->
             "SubjectFromWebIdentityToken", String.to_json f)
       ; Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  ; subject_from_web_identity_token =
      Aws.Util.option_map (Aws.Json.lookup j "SubjectFromWebIdentityToken") String.of_json
  ; assumed_role_user =
      Aws.Util.option_map (Aws.Json.lookup j "AssumedRoleUser") AssumedRoleUser.of_json
  ; packed_policy_size =
      Aws.Util.option_map (Aws.Json.lookup j "PackedPolicySize") Integer.of_json
  ; provider = Aws.Util.option_map (Aws.Json.lookup j "Provider") String.of_json
  ; audience = Aws.Util.option_map (Aws.Json.lookup j "Audience") String.of_json
  ; source_identity =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentity") String.of_json
  }
