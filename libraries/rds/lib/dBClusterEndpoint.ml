open Aws.BaseTypes

type t =
  { d_b_cluster_endpoint_identifier : String.t option
  ; d_b_cluster_identifier : String.t option
  ; d_b_cluster_endpoint_resource_identifier : String.t option
  ; endpoint : String.t option
  ; status : String.t option
  ; endpoint_type : String.t option
  ; custom_endpoint_type : String.t option
  ; static_members : StringList.t
  ; excluded_members : StringList.t
  ; d_b_cluster_endpoint_arn : String.t option
  }

let make
    ?d_b_cluster_endpoint_identifier
    ?d_b_cluster_identifier
    ?d_b_cluster_endpoint_resource_identifier
    ?endpoint
    ?status
    ?endpoint_type
    ?custom_endpoint_type
    ?(static_members = [])
    ?(excluded_members = [])
    ?d_b_cluster_endpoint_arn
    () =
  { d_b_cluster_endpoint_identifier
  ; d_b_cluster_identifier
  ; d_b_cluster_endpoint_resource_identifier
  ; endpoint
  ; status
  ; endpoint_type
  ; custom_endpoint_type
  ; static_members
  ; excluded_members
  ; d_b_cluster_endpoint_arn
  }

let parse xml =
  Some
    { d_b_cluster_endpoint_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterEndpointIdentifier" xml)
          String.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; d_b_cluster_endpoint_resource_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterEndpointResourceIdentifier" xml)
          String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; endpoint_type =
        Aws.Util.option_bind (Aws.Xml.member "EndpointType" xml) String.parse
    ; custom_endpoint_type =
        Aws.Util.option_bind (Aws.Xml.member "CustomEndpointType" xml) String.parse
    ; static_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StaticMembers" xml) StringList.parse)
    ; excluded_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExcludedMembers" xml) StringList.parse)
    ; d_b_cluster_endpoint_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterEndpointArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_endpoint_arn (fun f ->
             Aws.Query.Pair ("DBClusterEndpointArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ExcludedMembers.member", StringList.to_query v.excluded_members))
       ; Some
           (Aws.Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
       ; Aws.Util.option_map v.custom_endpoint_type (fun f ->
             Aws.Query.Pair ("CustomEndpointType", String.to_query f))
       ; Aws.Util.option_map v.endpoint_type (fun f ->
             Aws.Query.Pair ("EndpointType", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_endpoint_resource_identifier (fun f ->
             Aws.Query.Pair ("DBClusterEndpointResourceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
             Aws.Query.Pair ("DBClusterEndpointIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_endpoint_arn (fun f ->
             "DBClusterEndpointArn", String.to_json f)
       ; Some ("ExcludedMembers", StringList.to_json v.excluded_members)
       ; Some ("StaticMembers", StringList.to_json v.static_members)
       ; Aws.Util.option_map v.custom_endpoint_type (fun f ->
             "CustomEndpointType", String.to_json f)
       ; Aws.Util.option_map v.endpoint_type (fun f -> "EndpointType", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_endpoint_resource_identifier (fun f ->
             "DBClusterEndpointResourceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_endpoint_identifier (fun f ->
             "DBClusterEndpointIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_endpoint_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterEndpointIdentifier") String.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; d_b_cluster_endpoint_resource_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterEndpointResourceIdentifier")
        String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; endpoint_type = Aws.Util.option_map (Aws.Json.lookup j "EndpointType") String.of_json
  ; custom_endpoint_type =
      Aws.Util.option_map (Aws.Json.lookup j "CustomEndpointType") String.of_json
  ; static_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StaticMembers"))
  ; excluded_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedMembers"))
  ; d_b_cluster_endpoint_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterEndpointArn") String.of_json
  }
