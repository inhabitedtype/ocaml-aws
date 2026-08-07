open Aws.BaseTypes

type t =
  { d_b_proxy_endpoint_name : String.t option
  ; d_b_proxy_endpoint_arn : String.t option
  ; d_b_proxy_name : String.t option
  ; status : DBProxyEndpointStatus.t option
  ; vpc_id : String.t option
  ; vpc_security_group_ids : StringList.t
  ; vpc_subnet_ids : StringList.t
  ; endpoint : String.t option
  ; created_date : DateTime.t option
  ; target_role : DBProxyEndpointTargetRole.t option
  ; is_default : Boolean.t option
  ; endpoint_network_type : EndpointNetworkType.t option
  }

let make
    ?d_b_proxy_endpoint_name
    ?d_b_proxy_endpoint_arn
    ?d_b_proxy_name
    ?status
    ?vpc_id
    ?(vpc_security_group_ids = [])
    ?(vpc_subnet_ids = [])
    ?endpoint
    ?created_date
    ?target_role
    ?is_default
    ?endpoint_network_type
    () =
  { d_b_proxy_endpoint_name
  ; d_b_proxy_endpoint_arn
  ; d_b_proxy_name
  ; status
  ; vpc_id
  ; vpc_security_group_ids
  ; vpc_subnet_ids
  ; endpoint
  ; created_date
  ; target_role
  ; is_default
  ; endpoint_network_type
  }

let parse xml =
  Some
    { d_b_proxy_endpoint_name =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpointName" xml) String.parse
    ; d_b_proxy_endpoint_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpointArn" xml) String.parse
    ; d_b_proxy_name =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) DBProxyEndpointStatus.parse
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
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "CreatedDate" xml) DateTime.parse
    ; target_role =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetRole" xml)
          DBProxyEndpointTargetRole.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "IsDefault" xml) Boolean.parse
    ; endpoint_network_type =
        Aws.Util.option_bind
          (Aws.Xml.member "EndpointNetworkType" xml)
          EndpointNetworkType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.endpoint_network_type (fun f ->
             Aws.Query.Pair ("EndpointNetworkType", EndpointNetworkType.to_query f))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.target_role (fun f ->
             Aws.Query.Pair ("TargetRole", DBProxyEndpointTargetRole.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Some
           (Aws.Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("VpcSecurityGroupIds.member", StringList.to_query v.vpc_security_group_ids))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", DBProxyEndpointStatus.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_name (fun f ->
             Aws.Query.Pair ("DBProxyName", String.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_endpoint_arn (fun f ->
             Aws.Query.Pair ("DBProxyEndpointArn", String.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_endpoint_name (fun f ->
             Aws.Query.Pair ("DBProxyEndpointName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.endpoint_network_type (fun f ->
             "EndpointNetworkType", EndpointNetworkType.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "IsDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.target_role (fun f ->
             "TargetRole", DBProxyEndpointTargetRole.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "CreatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Some ("VpcSubnetIds", StringList.to_json v.vpc_subnet_ids)
       ; Some ("VpcSecurityGroupIds", StringList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", DBProxyEndpointStatus.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_name (fun f -> "DBProxyName", String.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_endpoint_arn (fun f ->
             "DBProxyEndpointArn", String.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_endpoint_name (fun f ->
             "DBProxyEndpointName", String.to_json f)
       ])

let of_json j =
  { d_b_proxy_endpoint_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBProxyEndpointName") String.of_json
  ; d_b_proxy_endpoint_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBProxyEndpointArn") String.of_json
  ; d_b_proxy_name = Aws.Util.option_map (Aws.Json.lookup j "DBProxyName") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") DBProxyEndpointStatus.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; vpc_security_group_ids =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; vpc_subnet_ids =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSubnetIds"))
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "CreatedDate") DateTime.of_json
  ; target_role =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetRole")
        DBProxyEndpointTargetRole.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "IsDefault") Boolean.of_json
  ; endpoint_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "EndpointNetworkType")
        EndpointNetworkType.of_json
  }
