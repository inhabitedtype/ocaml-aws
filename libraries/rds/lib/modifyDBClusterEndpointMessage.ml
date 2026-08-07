open Aws.BaseTypes

type t =
  { d_b_cluster_endpoint_identifier : String.t
  ; endpoint_type : String.t option
  ; static_members : StringList.t
  ; excluded_members : StringList.t
  }

let make
    ~d_b_cluster_endpoint_identifier
    ?endpoint_type
    ?(static_members = [])
    ?(excluded_members = [])
    () =
  { d_b_cluster_endpoint_identifier; endpoint_type; static_members; excluded_members }

let parse xml =
  Some
    { d_b_cluster_endpoint_identifier =
        Aws.Xml.required
          "DBClusterEndpointIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterEndpointIdentifier" xml)
             String.parse)
    ; endpoint_type =
        Aws.Util.option_bind (Aws.Xml.member "EndpointType" xml) String.parse
    ; static_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StaticMembers" xml) StringList.parse)
    ; excluded_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExcludedMembers" xml) StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ExcludedMembers.member", StringList.to_query v.excluded_members))
       ; Some
           (Aws.Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
       ; Aws.Util.option_map v.endpoint_type (fun f ->
             Aws.Query.Pair ("EndpointType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterEndpointIdentifier"
              , String.to_query v.d_b_cluster_endpoint_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ExcludedMembers", StringList.to_json v.excluded_members)
       ; Some ("StaticMembers", StringList.to_json v.static_members)
       ; Aws.Util.option_map v.endpoint_type (fun f -> "EndpointType", String.to_json f)
       ; Some
           ( "DBClusterEndpointIdentifier"
           , String.to_json v.d_b_cluster_endpoint_identifier )
       ])

let of_json j =
  { d_b_cluster_endpoint_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterEndpointIdentifier"))
  ; endpoint_type = Aws.Util.option_map (Aws.Json.lookup j "EndpointType") String.of_json
  ; static_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StaticMembers"))
  ; excluded_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedMembers"))
  }
