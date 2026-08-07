open Aws.BaseTypes

type t =
  { marker : String.t option
  ; d_b_cluster_backtracks : DBClusterBacktrackList.t
  }

let make ?marker ?(d_b_cluster_backtracks = []) () = { marker; d_b_cluster_backtracks }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; d_b_cluster_backtracks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterBacktracks" xml)
             DBClusterBacktrackList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DBClusterBacktracks.member"
              , DBClusterBacktrackList.to_query v.d_b_cluster_backtracks ))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("DBClusterBacktracks", DBClusterBacktrackList.to_json v.d_b_cluster_backtracks)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; d_b_cluster_backtracks =
      DBClusterBacktrackList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterBacktracks"))
  }
