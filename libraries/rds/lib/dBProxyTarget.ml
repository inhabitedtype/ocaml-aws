open Aws.BaseTypes

type t =
  { target_arn : String.t option
  ; endpoint : String.t option
  ; tracked_cluster_id : String.t option
  ; rds_resource_id : String.t option
  ; port : Integer.t option
  ; type_ : TargetType.t option
  ; role : TargetRole.t option
  ; target_health : TargetHealth.t option
  }

let make
    ?target_arn
    ?endpoint
    ?tracked_cluster_id
    ?rds_resource_id
    ?port
    ?type_
    ?role
    ?target_health
    () =
  { target_arn
  ; endpoint
  ; tracked_cluster_id
  ; rds_resource_id
  ; port
  ; type_
  ; role
  ; target_health
  }

let parse xml =
  Some
    { target_arn = Aws.Util.option_bind (Aws.Xml.member "TargetArn" xml) String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; tracked_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "TrackedClusterId" xml) String.parse
    ; rds_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "RdsResourceId" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) TargetType.parse
    ; role = Aws.Util.option_bind (Aws.Xml.member "Role" xml) TargetRole.parse
    ; target_health =
        Aws.Util.option_bind (Aws.Xml.member "TargetHealth" xml) TargetHealth.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_health (fun f ->
             Aws.Query.Pair ("TargetHealth", TargetHealth.to_query f))
       ; Aws.Util.option_map v.role (fun f ->
             Aws.Query.Pair ("Role", TargetRole.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", TargetType.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.rds_resource_id (fun f ->
             Aws.Query.Pair ("RdsResourceId", String.to_query f))
       ; Aws.Util.option_map v.tracked_cluster_id (fun f ->
             Aws.Query.Pair ("TrackedClusterId", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.target_arn (fun f ->
             Aws.Query.Pair ("TargetArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_health (fun f ->
             "TargetHealth", TargetHealth.to_json f)
       ; Aws.Util.option_map v.role (fun f -> "Role", TargetRole.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", TargetType.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.rds_resource_id (fun f ->
             "RdsResourceId", String.to_json f)
       ; Aws.Util.option_map v.tracked_cluster_id (fun f ->
             "TrackedClusterId", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.target_arn (fun f -> "TargetArn", String.to_json f)
       ])

let of_json j =
  { target_arn = Aws.Util.option_map (Aws.Json.lookup j "TargetArn") String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; tracked_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "TrackedClusterId") String.of_json
  ; rds_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "RdsResourceId") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") TargetType.of_json
  ; role = Aws.Util.option_map (Aws.Json.lookup j "Role") TargetRole.of_json
  ; target_health =
      Aws.Util.option_map (Aws.Json.lookup j "TargetHealth") TargetHealth.of_json
  }
