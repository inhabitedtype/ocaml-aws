open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t
  ; engine_family : EngineFamily.t
  ; default_auth_scheme : DefaultAuthScheme.t option
  ; auth : UserAuthConfigList.t
  ; role_arn : String.t
  ; vpc_subnet_ids : StringList.t
  ; vpc_security_group_ids : StringList.t
  ; require_t_l_s : Boolean.t option
  ; idle_client_timeout : Integer.t option
  ; debug_logging : Boolean.t option
  ; tags : TagList.t
  ; endpoint_network_type : EndpointNetworkType.t option
  ; target_connection_network_type : TargetConnectionNetworkType.t option
  }

let make
    ~d_b_proxy_name
    ~engine_family
    ?default_auth_scheme
    ?(auth = [])
    ~role_arn
    ~vpc_subnet_ids
    ?(vpc_security_group_ids = [])
    ?require_t_l_s
    ?idle_client_timeout
    ?debug_logging
    ?(tags = [])
    ?endpoint_network_type
    ?target_connection_network_type
    () =
  { d_b_proxy_name
  ; engine_family
  ; default_auth_scheme
  ; auth
  ; role_arn
  ; vpc_subnet_ids
  ; vpc_security_group_ids
  ; require_t_l_s
  ; idle_client_timeout
  ; debug_logging
  ; tags
  ; endpoint_network_type
  ; target_connection_network_type
  }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    ; engine_family =
        Aws.Xml.required
          "EngineFamily"
          (Aws.Util.option_bind (Aws.Xml.member "EngineFamily" xml) EngineFamily.parse)
    ; default_auth_scheme =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultAuthScheme" xml)
          DefaultAuthScheme.parse
    ; auth =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Auth" xml) UserAuthConfigList.parse)
    ; role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; vpc_subnet_ids =
        Aws.Xml.required
          "VpcSubnetIds"
          (Aws.Util.option_bind (Aws.Xml.member "VpcSubnetIds" xml) StringList.parse)
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             StringList.parse)
    ; require_t_l_s = Aws.Util.option_bind (Aws.Xml.member "RequireTLS" xml) Boolean.parse
    ; idle_client_timeout =
        Aws.Util.option_bind (Aws.Xml.member "IdleClientTimeout" xml) Integer.parse
    ; debug_logging =
        Aws.Util.option_bind (Aws.Xml.member "DebugLogging" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; endpoint_network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "EndpointNetworkType" xml)
          EndpointNetworkType.parse
    ; target_connection_network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetConnectionNetworkType" xml)
          TargetConnectionNetworkType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_connection_network_type (fun f ->
             Aws.Query.Pair
               ("TargetConnectionNetworkType", TargetConnectionNetworkType.to_query f))
       ; Aws.Util.option_map v.endpoint_network_type (fun f ->
             Aws.Query.Pair ("EndpointNetworkType", EndpointNetworkType.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.debug_logging (fun f ->
             Aws.Query.Pair ("DebugLogging", Boolean.to_query f))
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             Aws.Query.Pair ("IdleClientTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.require_t_l_s (fun f ->
             Aws.Query.Pair ("RequireTLS", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VpcSecurityGroupIds.member", StringList.to_query v.vpc_security_group_ids))
       ; Some
           (Aws.Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ; Some (Aws.Query.Pair ("Auth.member", UserAuthConfigList.to_query v.auth))
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             Aws.Query.Pair ("DefaultAuthScheme", DefaultAuthScheme.to_query f))
       ; Some (Aws.Query.Pair ("EngineFamily", EngineFamily.to_query v.engine_family))
       ; Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_connection_network_type (fun f ->
             "TargetConnectionNetworkType", TargetConnectionNetworkType.to_json f)
       ; Aws.Util.option_map v.endpoint_network_type (fun f ->
             "EndpointNetworkType", EndpointNetworkType.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.debug_logging (fun f -> "DebugLogging", Boolean.to_json f)
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             "IdleClientTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.require_t_l_s (fun f -> "RequireTLS", Boolean.to_json f)
       ; Some ("VpcSecurityGroupIds", StringList.to_json v.vpc_security_group_ids)
       ; Some ("VpcSubnetIds", StringList.to_json v.vpc_subnet_ids)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ; Some ("Auth", UserAuthConfigList.to_json v.auth)
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             "DefaultAuthScheme", DefaultAuthScheme.to_json f)
       ; Some ("EngineFamily", EngineFamily.to_json v.engine_family)
       ; Some ("DBProxyName", String.to_json v.d_b_proxy_name)
       ])

let of_json j =
  { d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  ; engine_family =
      EngineFamily.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineFamily"))
  ; default_auth_scheme =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultAuthScheme")
        DefaultAuthScheme.of_json
  ; auth = UserAuthConfigList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Auth"))
  ; role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; vpc_subnet_ids =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSubnetIds"))
  ; vpc_security_group_ids =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; require_t_l_s = Aws.Util.option_map (Aws.Json.lookup j "RequireTLS") Boolean.of_json
  ; idle_client_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "IdleClientTimeout") Integer.of_json
  ; debug_logging = Aws.Util.option_map (Aws.Json.lookup j "DebugLogging") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; endpoint_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "EndpointNetworkType")
        EndpointNetworkType.of_json
  ; target_connection_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetConnectionNetworkType")
        TargetConnectionNetworkType.of_json
  }
