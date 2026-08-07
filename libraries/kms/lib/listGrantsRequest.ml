open Aws.BaseTypes

type t =
  { limit : Integer.t option
  ; marker : String.t option
  ; key_id : String.t
  ; grant_id : String.t option
  ; grantee_principal : String.t option
  ; grantee_service_principal : String.t option
  }

let make ?limit ?marker ~key_id ?grant_id ?grantee_principal ?grantee_service_principal ()
    =
  { limit; marker; key_id; grant_id; grantee_principal; grantee_service_principal }

let parse xml =
  Some
    { limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
    ; grantee_principal =
        Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse
    ; grantee_service_principal =
        Aws.Util.option_bind (Aws.Xml.member "GranteeServicePrincipal" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.grantee_service_principal (fun f ->
             Aws.Query.Pair ("GranteeServicePrincipal", String.to_query f))
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             Aws.Query.Pair ("GranteePrincipal", String.to_query f))
       ; Aws.Util.option_map v.grant_id (fun f ->
             Aws.Query.Pair ("GrantId", String.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.limit (fun f ->
             Aws.Query.Pair ("Limit", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.grantee_service_principal (fun f ->
             "GranteeServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.grantee_principal (fun f ->
             "GranteePrincipal", String.to_json f)
       ; Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
       ])

let of_json j =
  { limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
  ; grantee_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteePrincipal") String.of_json
  ; grantee_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "GranteeServicePrincipal") String.of_json
  }
