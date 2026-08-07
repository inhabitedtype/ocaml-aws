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
    { issuer = Aws.Util.option_bind (Aws.Xml.member "issuer" xml) String.parse
    ; authorization_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "authorizationEndpoint" xml) String.parse
    ; token_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "tokenEndpoint" xml) String.parse
    ; user_info_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "userInfoEndpoint" xml) String.parse
    ; client_id = Aws.Util.option_bind (Aws.Xml.member "clientId" xml) String.parse
    ; client_secret =
        Aws.Util.option_bind (Aws.Xml.member "clientSecret" xml) String.parse
    ; scope = Aws.Util.option_bind (Aws.Xml.member "scope" xml) String.parse
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
       [ Aws.Util.option_map v.scope (fun f -> "scope", String.to_json f)
       ; Aws.Util.option_map v.client_secret (fun f -> "clientSecret", String.to_json f)
       ; Aws.Util.option_map v.client_id (fun f -> "clientId", String.to_json f)
       ; Aws.Util.option_map v.user_info_endpoint (fun f ->
             "userInfoEndpoint", String.to_json f)
       ; Aws.Util.option_map v.token_endpoint (fun f -> "tokenEndpoint", String.to_json f)
       ; Aws.Util.option_map v.authorization_endpoint (fun f ->
             "authorizationEndpoint", String.to_json f)
       ; Aws.Util.option_map v.issuer (fun f -> "issuer", String.to_json f)
       ])

let of_json j =
  { issuer = Aws.Util.option_map (Aws.Json.lookup j "issuer") String.of_json
  ; authorization_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "authorizationEndpoint") String.of_json
  ; token_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "tokenEndpoint") String.of_json
  ; user_info_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "userInfoEndpoint") String.of_json
  ; client_id = Aws.Util.option_map (Aws.Json.lookup j "clientId") String.of_json
  ; client_secret = Aws.Util.option_map (Aws.Json.lookup j "clientSecret") String.of_json
  ; scope = Aws.Util.option_map (Aws.Json.lookup j "scope") String.of_json
  }
