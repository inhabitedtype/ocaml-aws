open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t option
  ; d_b_proxy_arn : String.t option
  ; status : DBProxyStatus.t option
  ; engine_family : String.t option
  ; vpc_id : String.t option
  ; vpc_security_group_ids : StringList.t
  ; vpc_subnet_ids : StringList.t
  ; default_auth_scheme : String.t option
  ; auth : UserAuthConfigInfoList.t
  ; role_arn : String.t option
  ; endpoint : String.t option
  ; require_t_l_s : Boolean.t option
  ; idle_client_timeout : Integer.t option
  ; debug_logging : Boolean.t option
  ; created_date : DateTime.t option
  ; updated_date : DateTime.t option
  ; endpoint_network_type : EndpointNetworkType.t option
  ; target_connection_network_type : TargetConnectionNetworkType.t option
  }

let make
    ?d_b_proxy_name
    ?d_b_proxy_arn
    ?status
    ?engine_family
    ?vpc_id
    ?(vpc_security_group_ids = [])
    ?(vpc_subnet_ids = [])
    ?default_auth_scheme
    ?(auth = [])
    ?role_arn
    ?endpoint
    ?require_t_l_s
    ?idle_client_timeout
    ?debug_logging
    ?created_date
    ?updated_date
    ?endpoint_network_type
    ?target_connection_network_type
    () =
  { d_b_proxy_name
  ; d_b_proxy_arn
  ; status
  ; engine_family
  ; vpc_id
  ; vpc_security_group_ids
  ; vpc_subnet_ids
  ; default_auth_scheme
  ; auth
  ; role_arn
  ; endpoint
  ; require_t_l_s
  ; idle_client_timeout
  ; debug_logging
  ; created_date
  ; updated_date
  ; endpoint_network_type
  ; target_connection_network_type
  }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse
    ; d_b_proxy_arn = Aws.Util.option_bind (Aws.Xml.member "DBProxyArn" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) DBProxyStatus.parse
    ; engine_family =
        Aws.Util.option_bind (Aws.Xml.member "EngineFamily" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             StringList.parse)
    ; vpc_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "VpcSubnetIds" xml) StringList.parse)
    ; default_auth_scheme =
        Aws.Util.option_bind (Aws.Xml.member "DefaultAuthScheme" xml) String.parse
    ; auth =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Auth" xml) UserAuthConfigInfoList.parse)
    ; role_arn = Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; require_t_l_s = Aws.Util.option_bind (Aws.Xml.member "RequireTLS" xml) Boolean.parse
    ; idle_client_timeout =
        Aws.Util.option_bind (Aws.Xml.member "IdleClientTimeout" xml) Integer.parse
    ; debug_logging =
        Aws.Util.option_bind (Aws.Xml.member "DebugLogging" xml) Boolean.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "CreatedDate" xml) DateTime.parse
    ; updated_date =
        Aws.Util.option_bind (Aws.Xml.member "UpdatedDate" xml) DateTime.parse
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
       ; Aws.Util.option_map v.updated_date (fun f ->
             Aws.Query.Pair ("UpdatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.debug_logging (fun f ->
             Aws.Query.Pair ("DebugLogging", Boolean.to_query f))
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             Aws.Query.Pair ("IdleClientTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.require_t_l_s (fun f ->
             Aws.Query.Pair ("RequireTLS", Boolean.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.role_arn (fun f ->
             Aws.Query.Pair ("RoleArn", String.to_query f))
       ; Some (Aws.Query.Pair ("Auth.member", UserAuthConfigInfoList.to_query v.auth))
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             Aws.Query.Pair ("DefaultAuthScheme", String.to_query f))
       ; Some
           (Aws.Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("VpcSecurityGroupIds.member", StringList.to_query v.vpc_security_group_ids))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.engine_family (fun f ->
             Aws.Query.Pair ("EngineFamily", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", DBProxyStatus.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_arn (fun f ->
             Aws.Query.Pair ("DBProxyArn", String.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_name (fun f ->
             Aws.Query.Pair ("DBProxyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_connection_network_type (fun f ->
             "TargetConnectionNetworkType", TargetConnectionNetworkType.to_json f)
       ; Aws.Util.option_map v.endpoint_network_type (fun f ->
             "EndpointNetworkType", EndpointNetworkType.to_json f)
       ; Aws.Util.option_map v.updated_date (fun f -> "UpdatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "CreatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.debug_logging (fun f -> "DebugLogging", Boolean.to_json f)
       ; Aws.Util.option_map v.idle_client_timeout (fun f ->
             "IdleClientTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.require_t_l_s (fun f -> "RequireTLS", Boolean.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.role_arn (fun f -> "RoleArn", String.to_json f)
       ; Some ("Auth", UserAuthConfigInfoList.to_json v.auth)
       ; Aws.Util.option_map v.default_auth_scheme (fun f ->
             "DefaultAuthScheme", String.to_json f)
       ; Some ("VpcSubnetIds", StringList.to_json v.vpc_subnet_ids)
       ; Some ("VpcSecurityGroupIds", StringList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.engine_family (fun f -> "EngineFamily", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", DBProxyStatus.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_arn (fun f -> "DBProxyArn", String.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_name (fun f -> "DBProxyName", String.to_json f)
       ])

let of_json j =
  { d_b_proxy_name = Aws.Util.option_map (Aws.Json.lookup j "DBProxyName") String.of_json
  ; d_b_proxy_arn = Aws.Util.option_map (Aws.Json.lookup j "DBProxyArn") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") DBProxyStatus.of_json
  ; engine_family = Aws.Util.option_map (Aws.Json.lookup j "EngineFamily") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; vpc_security_group_ids =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; vpc_subnet_ids =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSubnetIds"))
  ; default_auth_scheme =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultAuthScheme") String.of_json
  ; auth =
      UserAuthConfigInfoList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Auth"))
  ; role_arn = Aws.Util.option_map (Aws.Json.lookup j "RoleArn") String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; require_t_l_s = Aws.Util.option_map (Aws.Json.lookup j "RequireTLS") Boolean.of_json
  ; idle_client_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "IdleClientTimeout") Integer.of_json
  ; debug_logging = Aws.Util.option_map (Aws.Json.lookup j "DebugLogging") Boolean.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "CreatedDate") DateTime.of_json
  ; updated_date = Aws.Util.option_map (Aws.Json.lookup j "UpdatedDate") DateTime.of_json
  ; endpoint_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "EndpointNetworkType")
        EndpointNetworkType.of_json
  ; target_connection_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetConnectionNetworkType")
        TargetConnectionNetworkType.of_json
  }
