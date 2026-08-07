open Aws.BaseTypes

type t =
  { protocol : VerifiedAccessEndpointProtocol.t option
  ; port : Integer.t option
  ; rds_db_instance_arn : String.t option
  ; rds_db_cluster_arn : String.t option
  ; rds_db_proxy_arn : String.t option
  ; rds_endpoint : String.t option
  ; subnet_ids : CreateVerifiedAccessEndpointSubnetIdList.t
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
          (Aws.Xml.member "Protocol" xml)
          VerifiedAccessEndpointProtocol.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; rds_db_instance_arn =
        Aws.Util.option_bind (Aws.Xml.member "RdsDbInstanceArn" xml) String.parse
    ; rds_db_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "RdsDbClusterArn" xml) String.parse
    ; rds_db_proxy_arn =
        Aws.Util.option_bind (Aws.Xml.member "RdsDbProxyArn" xml) String.parse
    ; rds_endpoint = Aws.Util.option_bind (Aws.Xml.member "RdsEndpoint" xml) String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             CreateVerifiedAccessEndpointSubnetIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
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
       [ Some ("SubnetId", CreateVerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.rds_endpoint (fun f -> "RdsEndpoint", String.to_json f)
       ; Aws.Util.option_map v.rds_db_proxy_arn (fun f ->
             "RdsDbProxyArn", String.to_json f)
       ; Aws.Util.option_map v.rds_db_cluster_arn (fun f ->
             "RdsDbClusterArn", String.to_json f)
       ; Aws.Util.option_map v.rds_db_instance_arn (fun f ->
             "RdsDbInstanceArn", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.protocol (fun f ->
             "Protocol", VerifiedAccessEndpointProtocol.to_json f)
       ])

let of_json j =
  { protocol =
      Aws.Util.option_map
        (Aws.Json.lookup j "Protocol")
        VerifiedAccessEndpointProtocol.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; rds_db_instance_arn =
      Aws.Util.option_map (Aws.Json.lookup j "RdsDbInstanceArn") String.of_json
  ; rds_db_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "RdsDbClusterArn") String.of_json
  ; rds_db_proxy_arn =
      Aws.Util.option_map (Aws.Json.lookup j "RdsDbProxyArn") String.of_json
  ; rds_endpoint = Aws.Util.option_map (Aws.Json.lookup j "RdsEndpoint") String.of_json
  ; subnet_ids =
      CreateVerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  }
