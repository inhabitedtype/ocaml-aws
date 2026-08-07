open Aws.BaseTypes

type t =
  { issuer : String.t option
  ; authorization_endpoint : String.t option
  ; token_endpoint : String.t option
  ; user_info_endpoint : String.t option
  ; client_id : String.t option
  ; client_secret : String.t option
  ; scope : String.t option
  }

let make
    ?issuer
    ?authorization_endpoint
    ?token_endpoint
    ?user_info_endpoint
    ?client_id
    ?client_secret
    ?scope
    () =
  { issuer
  ; authorization_endpoint
  ; token_endpoint
  ; user_info_endpoint
  ; client_id
  ; client_secret
  ; scope
  }

let parse xml =
  Some
    { issuer = Aws.Util.option_bind (Aws.Xml.member "Issuer" xml) String.parse
    ; authorization_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "AuthorizationEndpoint" xml) String.parse
    ; token_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "TokenEndpoint" xml) String.parse
    ; user_info_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "UserInfoEndpoint" xml) String.parse
    ; client_id = Aws.Util.option_bind (Aws.Xml.member "ClientId" xml) String.parse
    ; client_secret =
        Aws.Util.option_bind (Aws.Xml.member "ClientSecret" xml) String.parse
    ; scope = Aws.Util.option_bind (Aws.Xml.member "Scope" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scope (fun f ->
             Aws.Query.Pair ("Scope", String.to_query f))
       ; Aws.Util.option_map v.client_secret (fun f ->
             Aws.Query.Pair ("ClientSecret", String.to_query f))
       ; Aws.Util.option_map v.client_id (fun f ->
             Aws.Query.Pair ("ClientId", String.to_query f))
       ; Aws.Util.option_map v.user_info_endpoint (fun f ->
             Aws.Query.Pair ("UserInfoEndpoint", String.to_query f))
       ; Aws.Util.option_map v.token_endpoint (fun f ->
             Aws.Query.Pair ("TokenEndpoint", String.to_query f))
       ; Aws.Util.option_map v.authorization_endpoint (fun f ->
             Aws.Query.Pair ("AuthorizationEndpoint", String.to_query f))
       ; Aws.Util.option_map v.issuer (fun f ->
             Aws.Query.Pair ("Issuer", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scope (fun f -> "Scope", String.to_json f)
       ; Aws.Util.option_map v.client_secret (fun f -> "ClientSecret", String.to_json f)
       ; Aws.Util.option_map v.client_id (fun f -> "ClientId", String.to_json f)
       ; Aws.Util.option_map v.user_info_endpoint (fun f ->
             "UserInfoEndpoint", String.to_json f)
       ; Aws.Util.option_map v.token_endpoint (fun f -> "TokenEndpoint", String.to_json f)
       ; Aws.Util.option_map v.authorization_endpoint (fun f ->
             "AuthorizationEndpoint", String.to_json f)
       ; Aws.Util.option_map v.issuer (fun f -> "Issuer", String.to_json f)
       ])

let of_json j =
  { issuer = Aws.Util.option_map (Aws.Json.lookup j "Issuer") String.of_json
  ; authorization_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "AuthorizationEndpoint") String.of_json
  ; token_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "TokenEndpoint") String.of_json
  ; user_info_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "UserInfoEndpoint") String.of_json
  ; client_id = Aws.Util.option_map (Aws.Json.lookup j "ClientId") String.of_json
  ; client_secret = Aws.Util.option_map (Aws.Json.lookup j "ClientSecret") String.of_json
  ; scope = Aws.Util.option_map (Aws.Json.lookup j "Scope") String.of_json
  }
