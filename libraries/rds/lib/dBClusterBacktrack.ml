open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t option
  ; backtrack_identifier : String.t option
  ; backtrack_to : DateTime.t option
  ; backtracked_from : DateTime.t option
  ; backtrack_request_creation_time : DateTime.t option
  ; status : String.t option
  }

let make
    ?d_b_cluster_identifier
    ?backtrack_identifier
    ?backtrack_to
    ?backtracked_from
    ?backtrack_request_creation_time
    ?status
    () =
  { d_b_cluster_identifier
  ; backtrack_identifier
  ; backtrack_to
  ; backtracked_from
  ; backtrack_request_creation_time
  ; status
  }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; backtrack_identifier =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackIdentifier" xml) String.parse
    ; backtrack_to =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackTo" xml) DateTime.parse
    ; backtracked_from =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackedFrom" xml) DateTime.parse
    ; backtrack_request_creation_time =
        Aws.Util.option_bind
          (Aws.Xml.member "BacktrackRequestCreationTime" xml)
          DateTime.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.backtrack_request_creation_time (fun f ->
             Aws.Query.Pair ("BacktrackRequestCreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.backtracked_from (fun f ->
             Aws.Query.Pair ("BacktrackedFrom", DateTime.to_query f))
       ; Aws.Util.option_map v.backtrack_to (fun f ->
             Aws.Query.Pair ("BacktrackTo", DateTime.to_query f))
       ; Aws.Util.option_map v.backtrack_identifier (fun f ->
             Aws.Query.Pair ("BacktrackIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.backtrack_request_creation_time (fun f ->
             "BacktrackRequestCreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.backtracked_from (fun f ->
             "BacktrackedFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.backtrack_to (fun f -> "BacktrackTo", DateTime.to_json f)
       ; Aws.Util.option_map v.backtrack_identifier (fun f ->
             "BacktrackIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; backtrack_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "BacktrackIdentifier") String.of_json
  ; backtrack_to = Aws.Util.option_map (Aws.Json.lookup j "BacktrackTo") DateTime.of_json
  ; backtracked_from =
      Aws.Util.option_map (Aws.Json.lookup j "BacktrackedFrom") DateTime.of_json
  ; backtrack_request_creation_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "BacktrackRequestCreationTime")
        DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
