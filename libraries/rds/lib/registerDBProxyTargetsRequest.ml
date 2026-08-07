open Aws.BaseTypes

type t =
  { d_b_proxy_name : String.t
  ; target_group_name : String.t option
  ; d_b_instance_identifiers : StringList.t
  ; d_b_cluster_identifiers : StringList.t
  }

let make
    ~d_b_proxy_name
    ?target_group_name
    ?(d_b_instance_identifiers = [])
    ?(d_b_cluster_identifiers = [])
    () =
  { d_b_proxy_name; target_group_name; d_b_instance_identifiers; d_b_cluster_identifiers }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    ; target_group_name =
        Aws.Util.option_bind (Aws.Xml.member "TargetGroupName" xml) String.parse
    ; d_b_instance_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBInstanceIdentifiers" xml)
             StringList.parse)
    ; d_b_cluster_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterIdentifiers" xml)
             StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DBClusterIdentifiers.member"
              , StringList.to_query v.d_b_cluster_identifiers ))
       ; Some
           (Aws.Query.Pair
              ( "DBInstanceIdentifiers.member"
              , StringList.to_query v.d_b_instance_identifiers ))
       ; Aws.Util.option_map v.target_group_name (fun f ->
             Aws.Query.Pair ("TargetGroupName", String.to_query f))
       ; Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DBClusterIdentifiers", StringList.to_json v.d_b_cluster_identifiers)
       ; Some ("DBInstanceIdentifiers", StringList.to_json v.d_b_instance_identifiers)
       ; Aws.Util.option_map v.target_group_name (fun f ->
             "TargetGroupName", String.to_json f)
       ; Some ("DBProxyName", String.to_json v.d_b_proxy_name)
       ])

let of_json j =
  { d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  ; target_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "TargetGroupName") String.of_json
  ; d_b_instance_identifiers =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifiers"))
  ; d_b_cluster_identifiers =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifiers"))
  }
