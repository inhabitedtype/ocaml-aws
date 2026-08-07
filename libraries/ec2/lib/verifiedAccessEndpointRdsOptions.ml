open Aws.BaseTypes

type t =
  { protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; rds_db_instance_arn : String.t option
  ; rds_db_cluster_arn : String.t option
  ; rds_db_proxy_arn : String.t option
  ; rds_endpoint : String.t option
  ; subnet_ids : VerifiedAccessEndpointSubnetIdList.t
  }

let make
    ?protocol
    ?port
    ?rds_db_instance_arn
    ?rds_db_cluster_arn
    ?rds_db_proxy_arn
    ?rds_endpoint
    ?(subnet_ids = [])
    () =
  { protocol
  ; port
  ; rds_db_instance_arn
  ; rds_db_cluster_arn
  ; rds_db_proxy_arn
  ; rds_endpoint
  ; subnet_ids
  }

let parse xml =
  Some
    { protocol =
        Aws.Util.option_bind
          (Aws.Xml.member "protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "port" xml) Integer.parse
    ; rds_db_instance_arn =
        Aws.Util.option_bind (Aws.Xml.member "rdsDbInstanceArn" xml) String.parse
    ; rds_db_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "rdsDbClusterArn" xml) String.parse
    ; rds_db_proxy_arn =
        Aws.Util.option_bind (Aws.Xml.member "rdsDbProxyArn" xml) String.parse
    ; rds_endpoint = Aws.Util.option_bind (Aws.Xml.member "rdsEndpoint" xml) String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subnetIdSet" xml)
             VerifiedAccessEndpointSubnetIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SubnetIdSet", VerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.rds_endpoint (fun f ->
             Aws.Query.Pair ("RdsEndpoint", String.to_query f))
       ; Aws.Util.option_map v.rds_db_proxy_arn (fun f ->
             Aws.Query.Pair ("RdsDbProxyArn", String.to_query f))
       ; Aws.Util.option_map v.rds_db_cluster_arn (fun f ->
             Aws.Query.Pair ("RdsDbClusterArn", String.to_query f))
       ; Aws.Util.option_map v.rds_db_instance_arn (fun f ->
             Aws.Query.Pair ("RdsDbInstanceArn", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", VerifiedAccessEndpointProtocol.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("subnetIdSet", VerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.rds_endpoint (fun f -> "rdsEndpoint", String.to_json f)
       ; Aws.Util.option_map v.rds_db_proxy_arn (fun f ->
             "rdsDbProxyArn", String.to_json f)
       ; Aws.Util.option_map v.rds_db_cluster_arn (fun f ->
             "rdsDbClusterArn", String.to_json f)
       ; Aws.Util.option_map v.rds_db_instance_arn (fun f ->
             "rdsDbInstanceArn", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "protocol", VerifiedAccessEndpointProtocol.to_json f)
       ])

let of_json j =
  { protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "port") Integer.of_json
  ; rds_db_instance_arn =
      Aws.Util.option_map (Aws.Json.lookup j "rdsDbInstanceArn") String.of_json
  ; rds_db_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "rdsDbClusterArn") String.of_json
  ; rds_db_proxy_arn =
      Aws.Util.option_map (Aws.Json.lookup j "rdsDbProxyArn") String.of_json
  ; rds_endpoint = Aws.Util.option_map (Aws.Json.lookup j "rdsEndpoint") String.of_json
  ; subnet_ids =
      VerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetIdSet"))
  }
