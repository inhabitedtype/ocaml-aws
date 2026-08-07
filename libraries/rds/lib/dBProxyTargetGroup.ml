open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t option
  ; target_group_name : String.t option
  ; target_group_arn : String.t option
  ; is_default : Boolean.t option
  ; status : String.t option
  ; connection_pool_config : ConnectionPoolConfigurationInfo.t option
  ; created_date : DateTime.t option
  ; updated_date : DateTime.t option
  }

let make
    ?d_b_proxy_name
    ?target_group_name
    ?target_group_arn
    ?is_default
    ?status
    ?connection_pool_config
    ?created_date
    ?updated_date
    () =
  { d_b_proxy_name
  ; target_group_name
  ; target_group_arn
  ; is_default
  ; status
  ; connection_pool_config
  ; created_date
  ; updated_date
  }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse
    ; target_group_name =
        Aws.Util.option_bind (Aws.Xml.member "TargetGroupName" xml) String.parse
    ; target_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "TargetGroupArn" xml) String.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "IsDefault" xml) Boolean.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; connection_pool_config =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionPoolConfig" xml)
          ConnectionPoolConfigurationInfo.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "CreatedDate" xml) DateTime.parse
    ; updated_date =
        Aws.Util.option_bind (Aws.Xml.member "UpdatedDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_date (fun f ->
             Aws.Query.Pair ("UpdatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.connection_pool_config (fun f ->
             Aws.Query.Pair
               ("ConnectionPoolConfig", ConnectionPoolConfigurationInfo.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.target_group_arn (fun f ->
             Aws.Query.Pair ("TargetGroupArn", String.to_query f))
       ; Aws.Util.option_map v.target_group_name (fun f ->
             Aws.Query.Pair ("TargetGroupName", String.to_query f))
       ; Aws.Util.option_map v.d_b_proxy_name (fun f ->
             Aws.Query.Pair ("DBProxyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_date (fun f -> "UpdatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "CreatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.connection_pool_config (fun f ->
             "ConnectionPoolConfig", ConnectionPoolConfigurationInfo.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "IsDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.target_group_arn (fun f ->
             "TargetGroupArn", String.to_json f)
       ; Aws.Util.option_map v.target_group_name (fun f ->
             "TargetGroupName", String.to_json f)
       ; Aws.Util.option_map v.d_b_proxy_name (fun f -> "DBProxyName", String.to_json f)
       ])

let of_json j =
  { d_b_proxy_name = Aws.Util.option_map (Aws.Json.lookup j "DBProxyName") String.of_json
  ; target_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "TargetGroupName") String.of_json
  ; target_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TargetGroupArn") String.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "IsDefault") Boolean.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; connection_pool_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionPoolConfig")
        ConnectionPoolConfigurationInfo.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "CreatedDate") DateTime.of_json
  ; updated_date = Aws.Util.option_map (Aws.Json.lookup j "UpdatedDate") DateTime.of_json
  }
