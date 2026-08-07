open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t option
  ; pending_capacity : Integer.t option
  ; current_capacity : Integer.t option
  ; seconds_before_timeout : Integer.t option
  ; timeout_action : String.t option
  }

let make
    ?d_b_cluster_identifier
    ?pending_capacity
    ?current_capacity
    ?seconds_before_timeout
    ?timeout_action
    () =
  { d_b_cluster_identifier
  ; pending_capacity
  ; current_capacity
  ; seconds_before_timeout
  ; timeout_action
  }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; pending_capacity =
        Aws.Util.option_bind (Aws.Xml.member "PendingCapacity" xml) Integer.parse
    ; current_capacity =
        Aws.Util.option_bind (Aws.Xml.member "CurrentCapacity" xml) Integer.parse
    ; seconds_before_timeout =
        Aws.Util.option_bind (Aws.Xml.member "SecondsBeforeTimeout" xml) Integer.parse
    ; timeout_action =
        Aws.Util.option_bind (Aws.Xml.member "TimeoutAction" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timeout_action (fun f ->
             Aws.Query.Pair ("TimeoutAction", String.to_query f))
       ; Aws.Util.option_map v.seconds_before_timeout (fun f ->
             Aws.Query.Pair ("SecondsBeforeTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.current_capacity (fun f ->
             Aws.Query.Pair ("CurrentCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.pending_capacity (fun f ->
             Aws.Query.Pair ("PendingCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timeout_action (fun f -> "TimeoutAction", String.to_json f)
       ; Aws.Util.option_map v.seconds_before_timeout (fun f ->
             "SecondsBeforeTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.current_capacity (fun f ->
             "CurrentCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.pending_capacity (fun f ->
             "PendingCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; pending_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "PendingCapacity") Integer.of_json
  ; current_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "CurrentCapacity") Integer.of_json
  ; seconds_before_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "SecondsBeforeTimeout") Integer.of_json
  ; timeout_action =
      Aws.Util.option_map (Aws.Json.lookup j "TimeoutAction") String.of_json
  }
