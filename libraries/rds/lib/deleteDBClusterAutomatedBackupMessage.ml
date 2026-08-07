open Aws.BaseTypes

type t = { db_cluster_resource_id : String.t }

let make ~db_cluster_resource_id () = { db_cluster_resource_id }

let parse xml =
  Some
    { db_cluster_resource_id =
        Aws.Xml.required
          "DbClusterResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "DbClusterResourceId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DbClusterResourceId", String.to_query v.db_cluster_resource_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DbClusterResourceId", String.to_json v.db_cluster_resource_id) ])

let of_json j =
  { db_cluster_resource_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DbClusterResourceId"))
  }
