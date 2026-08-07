open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t
  ; d_b_cluster_endpoint_identifier : String.t
  ; endpoint_type : String.t
  ; static_members : StringList.t
  ; excluded_members : StringList.t
  ; tags : TagList.t
  }

let make
    ~d_b_cluster_identifier
    ~d_b_cluster_endpoint_identifier
    ~endpoint_type
    ?(static_members = [])
    ?(excluded_members = [])
    ?(tags = [])
    () =
  { d_b_cluster_identifier
  ; d_b_cluster_endpoint_identifier
  ; endpoint_type
  ; static_members
  ; excluded_members
  ; tags
  }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; d_b_cluster_endpoint_identifier =
        Aws.Xml.required
          "DBClusterEndpointIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterEndpointIdentifier" xml)
             String.parse)
    ; endpoint_type =
        Aws.Xml.required
          "EndpointType"
          (Aws.Util.option_bind (Aws.Xml.member "EndpointType" xml) String.parse)
    ; static_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StaticMembers" xml) StringList.parse)
    ; excluded_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExcludedMembers" xml) StringList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("ExcludedMembers.member", StringList.to_query v.excluded_members))
       ; Some
           (Aws.Query.Pair ("StaticMembers.member", StringList.to_query v.static_members))
       ; Some (Aws.Query.Pair ("EndpointType", String.to_query v.endpoint_type))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterEndpointIdentifier"
              , String.to_query v.d_b_cluster_endpoint_identifier ))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("ExcludedMembers", StringList.to_json v.excluded_members)
       ; Some ("StaticMembers", StringList.to_json v.static_members)
       ; Some ("EndpointType", String.to_json v.endpoint_type)
       ; Some
           ( "DBClusterEndpointIdentifier"
           , String.to_json v.d_b_cluster_endpoint_identifier )
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ])

let of_json j =
  { d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; d_b_cluster_endpoint_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterEndpointIdentifier"))
  ; endpoint_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndpointType"))
  ; static_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StaticMembers"))
  ; excluded_members =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedMembers"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
