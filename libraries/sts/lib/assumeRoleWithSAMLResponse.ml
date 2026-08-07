open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; assumed_role_user : AssumedRoleUser.t option
  ; packed_policy_size : Integer.t option
  ; subject : String.t option
  ; subject_type : String.t option
  ; issuer : String.t option
  ; audience : String.t option
  ; name_qualifier : String.t option
  ; source_identity : String.t option
  }

let make
    ?credentials
    ?assumed_role_user
    ?packed_policy_size
    ?subject
    ?subject_type
    ?issuer
    ?audience
    ?name_qualifier
    ?source_identity
    () =
  { credentials
  ; assumed_role_user
  ; packed_policy_size
  ; subject
  ; subject_type
  ; issuer
  ; audience
  ; name_qualifier
  ; source_identity
  }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; assumed_role_user =
        Aws.Util.option_bind (Aws.Xml.member "AssumedRoleUser" xml) AssumedRoleUser.parse
    ; packed_policy_size =
        Aws.Util.option_bind (Aws.Xml.member "PackedPolicySize" xml) Integer.parse
    ; subject = Aws.Util.option_bind (Aws.Xml.member "Subject" xml) String.parse
    ; subject_type = Aws.Util.option_bind (Aws.Xml.member "SubjectType" xml) String.parse
    ; issuer = Aws.Util.option_bind (Aws.Xml.member "Issuer" xml) String.parse
    ; audience = Aws.Util.option_bind (Aws.Xml.member "Audience" xml) String.parse
    ; name_qualifier =
        Aws.Util.option_bind (Aws.Xml.member "NameQualifier" xml) String.parse
    ; source_identity =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             Aws.Query.Pair ("SourceIdentity", String.to_query f))
       ; Aws.Util.option_map v.name_qualifier (fun f ->
             Aws.Query.Pair ("NameQualifier", String.to_query f))
       ; Aws.Util.option_map v.audience (fun f ->
             Aws.Query.Pair ("Audience", String.to_query f))
       ; Aws.Util.option_map v.issuer (fun f ->
             Aws.Query.Pair ("Issuer", String.to_query f))
       ; Aws.Util.option_map v.subject_type (fun f ->
             Aws.Query.Pair ("SubjectType", String.to_query f))
       ; Aws.Util.option_map v.subject (fun f ->
             Aws.Query.Pair ("Subject", String.to_query f))
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
       ; Aws.Util.option_map v.name_qualifier (fun f -> "NameQualifier", String.to_json f)
       ; Aws.Util.option_map v.audience (fun f -> "Audience", String.to_json f)
       ; Aws.Util.option_map v.issuer (fun f -> "Issuer", String.to_json f)
       ; Aws.Util.option_map v.subject_type (fun f -> "SubjectType", String.to_json f)
       ; Aws.Util.option_map v.subject (fun f -> "Subject", String.to_json f)
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
  ; subject = Aws.Util.option_map (Aws.Json.lookup j "Subject") String.of_json
  ; subject_type = Aws.Util.option_map (Aws.Json.lookup j "SubjectType") String.of_json
  ; issuer = Aws.Util.option_map (Aws.Json.lookup j "Issuer") String.of_json
  ; audience = Aws.Util.option_map (Aws.Json.lookup j "Audience") String.of_json
  ; name_qualifier =
      Aws.Util.option_map (Aws.Json.lookup j "NameQualifier") String.of_json
  ; source_identity =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentity") String.of_json
  }
