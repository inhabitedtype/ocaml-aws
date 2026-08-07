open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t
  ; target_db_cluster_identifier : String.t
  }

let make ~global_cluster_identifier ~target_db_cluster_identifier () =
  { global_cluster_identifier; target_db_cluster_identifier }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Xml.required
          "GlobalClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusterIdentifier" xml)
             String.parse)
    ; target_db_cluster_identifier =
        Aws.Xml.required
          "TargetDbClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDbClusterIdentifier" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TargetDbClusterIdentifier", String.to_query v.target_db_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ("GlobalClusterIdentifier", String.to_query v.global_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TargetDbClusterIdentifier", String.to_json v.target_db_cluster_identifier)
       ; Some ("GlobalClusterIdentifier", String.to_json v.global_cluster_identifier)
       ])

let of_json j =
  { global_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusterIdentifier"))
  ; target_db_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetDbClusterIdentifier"))
  }
