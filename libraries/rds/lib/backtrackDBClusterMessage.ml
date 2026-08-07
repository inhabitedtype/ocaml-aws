open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t
  ; backtrack_to : DateTime.t
  ; force : Boolean.t option
  ; use_earliest_time_on_point_in_time_unavailable : Boolean.t option
  }

let make
    ~d_b_cluster_identifier
    ~backtrack_to
    ?force
    ?use_earliest_time_on_point_in_time_unavailable
    () =
  { d_b_cluster_identifier
  ; backtrack_to
  ; force
  ; use_earliest_time_on_point_in_time_unavailable
  }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; backtrack_to =
        Aws.Xml.required
          "BacktrackTo"
          (Aws.Util.option_bind (Aws.Xml.member "BacktrackTo" xml) DateTime.parse)
    ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
    ; use_earliest_time_on_point_in_time_unavailable =
        Aws.Util.option_bind
          (Aws.Xml.member "UseEarliestTimeOnPointInTimeUnavailable" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.use_earliest_time_on_point_in_time_unavailable (fun f ->
             Aws.Query.Pair ("UseEarliestTimeOnPointInTimeUnavailable", Boolean.to_query f))
       ; Aws.Util.option_map v.force (fun f ->
             Aws.Query.Pair ("Force", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("BacktrackTo", DateTime.to_query v.backtrack_to))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.use_earliest_time_on_point_in_time_unavailable (fun f ->
             "UseEarliestTimeOnPointInTimeUnavailable", Boolean.to_json f)
       ; Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
       ; Some ("BacktrackTo", DateTime.to_json v.backtrack_to)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ])

let of_json j =
  { d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; backtrack_to =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BacktrackTo"))
  ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
  ; use_earliest_time_on_point_in_time_unavailable =
      Aws.Util.option_map
        (Aws.Json.lookup j "UseEarliestTimeOnPointInTimeUnavailable")
        Boolean.of_json
  }
