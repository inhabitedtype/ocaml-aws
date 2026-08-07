open Aws.BaseTypes

type t = { d_b_cluster_endpoint_identifier : String.t }

let make ~d_b_cluster_endpoint_identifier () = { d_b_cluster_endpoint_identifier }

let parse xml =
  Some
    { d_b_cluster_endpoint_identifier =
        Aws.Xml.required
          "DBClusterEndpointIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterEndpointIdentifier" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DBClusterEndpointIdentifier"
              , String.to_query v.d_b_cluster_endpoint_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "DBClusterEndpointIdentifier"
           , String.to_json v.d_b_cluster_endpoint_identifier )
       ])

let of_json j =
  { d_b_cluster_endpoint_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterEndpointIdentifier"))
  }
