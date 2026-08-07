open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t
  ; db_cluster_identifier : String.t
  }

let make ~global_cluster_identifier ~db_cluster_identifier () =
  { global_cluster_identifier; db_cluster_identifier }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Xml.required
          "GlobalClusterIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusterIdentifier" xml)
             String.parse)
    ; db_cluster_identifier =
        Aws.Xml.required
          "DbClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DbClusterIdentifier" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("DbClusterIdentifier", String.to_query v.db_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ("GlobalClusterIdentifier", String.to_query v.global_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DbClusterIdentifier", String.to_json v.db_cluster_identifier)
       ; Some ("GlobalClusterIdentifier", String.to_json v.global_cluster_identifier)
       ])

let of_json j =
  { global_cluster_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusterIdentifier"))
  ; db_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DbClusterIdentifier"))
  }
