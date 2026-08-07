open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t
  ; new_d_b_proxy_name : String.t option
  ; default_auth_scheme : DefaultAuthScheme.t option
  ; auth : UserAuthConfigList.t
  ; require_t_l_s : Boolean.t option
  ; idle_client_timeout : Integer.t option
  ; debug_logging : Boolean.t option
  ; role_arn : String.t option
  ; security_groups : StringList.t
  }

let make
    ~d_b_proxy_name
    ?new_d_b_proxy_name
    ?default_auth_scheme
    ?(auth = [])
    ?require_t_l_s
    ?idle_client_timeout
    ?debug_logging
    ?role_arn
    ?(security_groups = [])
    () =
  { d_b_proxy_name
  ; new_d_b_proxy_name
  ; default_auth_scheme
  ; auth
  ; require_t_l_s
  ; idle_client_timeout
  ; debug_logging
  ; role_arn
  ; security_groups
  }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    ; new_d_b_proxy_name =
        Aws.Util.option_bind (Aws.Xml.member "NewDBProxyName" xml) String.parse
    ; default_auth_scheme =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultAuthScheme" xml)
          DefaultAuthScheme.parse
    ; auth =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Auth" xml) UserAuthConfigList.parse)
    ; require_t_l_s = Aws.Util.option_bind (Aws.Xml.member "RequireTLS" xml) Boolean.parse
    ; idle_client_timeout =
        Aws.Util.option_bind (Aws.Xml.member "IdleClientTimeout" xml) Integer.parse
    ; debug_logging =
        Aws.Util.option_bind (Aws.Xml.member "DebugLogging" xml) Boolean.parse
    ; role_arn = Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SecurityGroups" xml) StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SecurityGroups.member", StringList.to_query v.security_groups))
       ; Aws.Util.option_map v.role_arn (fun f ->
             Aws.Query.Pair ("RoleArn", String.to_query f))
       ; Aws.Util.option_map v.debug_logging (fun f ->
             Aws.Query.Pair ("DebugLogging", Boolean.to_query f))
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             Aws.Query.Pair ("IdleClientTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.require_t_l_s (fun f ->
             Aws.Query.Pair ("RequireTLS", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Auth.member", UserAuthConfigList.to_query v.auth))
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             Aws.Query.Pair ("DefaultAuthScheme", DefaultAuthScheme.to_query f))
       ; Aws.Util.option_map v.new_d_b_proxy_name (fun f ->
             Aws.Query.Pair ("NewDBProxyName", String.to_query f))
       ; Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SecurityGroups", StringList.to_json v.security_groups)
       ; Aws.Util.option_map v.role_arn (fun f -> "RoleArn", String.to_json f)
       ; Aws.Util.option_map v.debug_logging (fun f -> "DebugLogging", Boolean.to_json f)
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             "IdleClientTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.require_t_l_s (fun f -> "RequireTLS", Boolean.to_json f)
       ; Some ("Auth", UserAuthConfigList.to_json v.auth)
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             "DefaultAuthScheme", DefaultAuthScheme.to_json f)
       ; Aws.Util.option_map v.new_d_b_proxy_name (fun f ->
             "NewDBProxyName", String.to_json f)
       ; Some ("DBProxyName", String.to_json v.d_b_proxy_name)
       ])

let of_json j =
  { d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  ; new_d_b_proxy_name =
      Aws.Util.option_map (Aws.Json.lookup j "NewDBProxyName") String.of_json
  ; default_auth_scheme =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultAuthScheme")
        DefaultAuthScheme.of_json
  ; auth = UserAuthConfigList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Auth"))
  ; require_t_l_s = Aws.Util.option_map (Aws.Json.lookup j "RequireTLS") Boolean.of_json
  ; idle_client_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "IdleClientTimeout") Integer.of_json
  ; debug_logging = Aws.Util.option_map (Aws.Json.lookup j "DebugLogging") Boolean.of_json
  ; role_arn = Aws.Util.option_map (Aws.Json.lookup j "RoleArn") String.of_json
  ; security_groups =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  }
