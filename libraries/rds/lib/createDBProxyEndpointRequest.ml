open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t
  ; d_b_proxy_endpoint_name : String.t
  ; vpc_subnet_ids : StringList.t
  ; vpc_security_group_ids : StringList.t
  ; target_role : DBProxyEndpointTargetRole.t option
  ; tags : TagList.t
  ; endpoint_network_type : EndpointNetworkType.t option
  }

let make
    ~d_b_proxy_name
    ~d_b_proxy_endpoint_name
    ~vpc_subnet_ids
    ?(vpc_security_group_ids = [])
    ?target_role
    ?(tags = [])
    ?endpoint_network_type
    () =
  { d_b_proxy_name
  ; d_b_proxy_endpoint_name
  ; vpc_subnet_ids
  ; vpc_security_group_ids
  ; target_role
  ; tags
  ; endpoint_network_type
  }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    ; d_b_proxy_endpoint_name =
        Aws.Xml.required
          "DBProxyEndpointName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpointName" xml) String.parse)
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
    ; target_role =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetRole" xml)
          DBProxyEndpointTargetRole.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
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
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.target_role (fun f ->
             Aws.Query.Pair ("TargetRole", DBProxyEndpointTargetRole.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VpcSecurityGroupIds.member", StringList.to_query v.vpc_security_group_ids))
       ; Some
           (Aws.Query.Pair ("VpcSubnetIds.member", StringList.to_query v.vpc_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("DBProxyEndpointName", String.to_query v.d_b_proxy_endpoint_name))
       ; Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.endpoint_network_type (fun f ->
             "EndpointNetworkType", EndpointNetworkType.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.target_role (fun f ->
             "TargetRole", DBProxyEndpointTargetRole.to_json f)
       ; Some ("VpcSecurityGroupIds", StringList.to_json v.vpc_security_group_ids)
       ; Some ("VpcSubnetIds", StringList.to_json v.vpc_subnet_ids)
       ; Some ("DBProxyEndpointName", String.to_json v.d_b_proxy_endpoint_name)
       ; Some ("DBProxyName", String.to_json v.d_b_proxy_name)
       ])

let of_json j =
  { d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  ; d_b_proxy_endpoint_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyEndpointName"))
  ; vpc_subnet_ids =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSubnetIds"))
  ; vpc_security_group_ids =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; target_role =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetRole")
        DBProxyEndpointTargetRole.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; endpoint_network_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "EndpointNetworkType")
        EndpointNetworkType.of_json
  }
