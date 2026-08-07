open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t
  ; role_arn : String.t
  ; feature_name : String.t option
  }

let make ~d_b_cluster_identifier ~role_arn ?feature_name () =
  { d_b_cluster_identifier; role_arn; feature_name }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; feature_name = Aws.Util.option_bind (Aws.Xml.member "FeatureName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.feature_name (fun f ->
             Aws.Query.Pair ("FeatureName", String.to_query f))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.feature_name (fun f -> "FeatureName", String.to_json f)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ])

let of_json j =
  { d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; feature_name = Aws.Util.option_map (Aws.Json.lookup j "FeatureName") String.of_json
  }
