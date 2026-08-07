open Aws.BaseTypes

type t =
  { d_b_proxy_endpoint_name : String.t
  ; new_d_b_proxy_endpoint_name : String.t option
  ; vpc_security_group_ids : StringList.t
  }

let make
    ~d_b_proxy_endpoint_name
    ?new_d_b_proxy_endpoint_name
    ?(vpc_security_group_ids = [])
    () =
  { d_b_proxy_endpoint_name; new_d_b_proxy_endpoint_name; vpc_security_group_ids }

let parse xml =
  Some
    { d_b_proxy_endpoint_name =
        Aws.Xml.required
          "DBProxyEndpointName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpointName" xml) String.parse)
    ; new_d_b_proxy_endpoint_name =
        Aws.Util.option_bind (Aws.Xml.member "NewDBProxyEndpointName" xml) String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("VpcSecurityGroupIds.member", StringList.to_query v.vpc_security_group_ids))
       ; Aws.Util.option_map v.new_d_b_proxy_endpoint_name (fun f ->
             Aws.Query.Pair ("NewDBProxyEndpointName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBProxyEndpointName", String.to_query v.d_b_proxy_endpoint_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("VpcSecurityGroupIds", StringList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.new_d_b_proxy_endpoint_name (fun f ->
             "NewDBProxyEndpointName", String.to_json f)
       ; Some ("DBProxyEndpointName", String.to_json v.d_b_proxy_endpoint_name)
       ])

let of_json j =
  { d_b_proxy_endpoint_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyEndpointName"))
  ; new_d_b_proxy_endpoint_name =
      Aws.Util.option_map (Aws.Json.lookup j "NewDBProxyEndpointName") String.of_json
  ; vpc_security_group_ids =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  }
