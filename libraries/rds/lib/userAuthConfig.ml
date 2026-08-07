open Aws.BaseTypes

type t =
  { description : String.t option
  ; user_name : String.t option
  ; auth_scheme : AuthScheme.t option
  ; secret_arn : String.t option
  ; i_a_m_auth : IAMAuthMode.t option
  ; client_password_auth_type : ClientPasswordAuthType.t option
  }

let make
    ?description
    ?user_name
    ?auth_scheme
    ?secret_arn
    ?i_a_m_auth
    ?client_password_auth_type
    () =
  { description
  ; user_name
  ; auth_scheme
  ; secret_arn
  ; i_a_m_auth
  ; client_password_auth_type
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; user_name = Aws.Util.option_bind (Aws.Xml.member "UserName" xml) String.parse
    ; auth_scheme =
        Aws.Util.option_bind (Aws.Xml.member "AuthScheme" xml) AuthScheme.parse
    ; secret_arn = Aws.Util.option_bind (Aws.Xml.member "SecretArn" xml) String.parse
    ; i_a_m_auth = Aws.Util.option_bind (Aws.Xml.member "IAMAuth" xml) IAMAuthMode.parse
    ; client_password_auth_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ClientPasswordAuthType" xml)
          ClientPasswordAuthType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_password_auth_type (fun f ->
             Aws.Query.Pair ("ClientPasswordAuthType", ClientPasswordAuthType.to_query f))
       ; Aws.Util.option_map v.i_a_m_auth (fun f ->
             Aws.Query.Pair ("IAMAuth", IAMAuthMode.to_query f))
       ; Aws.Util.option_map v.secret_arn (fun f ->
             Aws.Query.Pair ("SecretArn", String.to_query f))
       ; Aws.Util.option_map v.auth_scheme (fun f ->
             Aws.Query.Pair ("AuthScheme", AuthScheme.to_query f))
       ; Aws.Util.option_map v.user_name (fun f ->
             Aws.Query.Pair ("UserName", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_password_auth_type (fun f ->
             "ClientPasswordAuthType", ClientPasswordAuthType.to_json f)
       ; Aws.Util.option_map v.i_a_m_auth (fun f -> "IAMAuth", IAMAuthMode.to_json f)
       ; Aws.Util.option_map v.secret_arn (fun f -> "SecretArn", String.to_json f)
       ; Aws.Util.option_map v.auth_scheme (fun f -> "AuthScheme", AuthScheme.to_json f)
       ; Aws.Util.option_map v.user_name (fun f -> "UserName", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; user_name = Aws.Util.option_map (Aws.Json.lookup j "UserName") String.of_json
  ; auth_scheme = Aws.Util.option_map (Aws.Json.lookup j "AuthScheme") AuthScheme.of_json
  ; secret_arn = Aws.Util.option_map (Aws.Json.lookup j "SecretArn") String.of_json
  ; i_a_m_auth = Aws.Util.option_map (Aws.Json.lookup j "IAMAuth") IAMAuthMode.of_json
  ; client_password_auth_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClientPasswordAuthType")
        ClientPasswordAuthType.of_json
  }
