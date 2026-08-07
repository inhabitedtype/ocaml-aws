open Aws.BaseTypes

type t =
  { target_group_name : String.t
  ; d_b_proxy_name : String.t
  ; connection_pool_config : ConnectionPoolConfiguration.t option
  ; new_name : String.t option
  }

let make ~target_group_name ~d_b_proxy_name ?connection_pool_config ?new_name () =
  { target_group_name; d_b_proxy_name; connection_pool_config; new_name }

let parse xml =
  Some
    { target_group_name =
        Aws.Xml.required
          "TargetGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "TargetGroupName" xml) String.parse)
    ; d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    ; connection_pool_config =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionPoolConfig" xml)
          ConnectionPoolConfiguration.parse
    ; new_name = Aws.Util.option_bind (Aws.Xml.member "NewName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.new_name (fun f ->
             Aws.Query.Pair ("NewName", String.to_query f))
       ; Aws.Util.option_map v.connection_pool_config (fun f ->
             Aws.Query.Pair
               ("ConnectionPoolConfig", ConnectionPoolConfiguration.to_query f))
       ; Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
       ; Some (Aws.Query.Pair ("TargetGroupName", String.to_query v.target_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.new_name (fun f -> "NewName", String.to_json f)
       ; Aws.Util.option_map v.connection_pool_config (fun f ->
             "ConnectionPoolConfig", ConnectionPoolConfiguration.to_json f)
       ; Some ("DBProxyName", String.to_json v.d_b_proxy_name)
       ; Some ("TargetGroupName", String.to_json v.target_group_name)
       ])

let of_json j =
  { target_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetGroupName"))
  ; d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  ; connection_pool_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionPoolConfig")
        ConnectionPoolConfiguration.of_json
  ; new_name = Aws.Util.option_map (Aws.Json.lookup j "NewName") String.of_json
  }
