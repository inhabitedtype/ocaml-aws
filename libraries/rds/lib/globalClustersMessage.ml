open Aws.BaseTypes

type t =
  { marker : String.t option
  ; global_clusters : GlobalClusterList.t
  }

let make ?marker ?(global_clusters = []) () = { marker; global_clusters }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; global_clusters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusters" xml)
             GlobalClusterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("GlobalClusters.member", GlobalClusterList.to_query v.global_clusters))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("GlobalClusters", GlobalClusterList.to_json v.global_clusters)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; global_clusters =
      GlobalClusterList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusters"))
  }
