open Aws.BaseTypes

type t =
  { d_b_cluster_identifier : String.t
  ; capacity : Integer.t option
  ; seconds_before_timeout : Integer.t option
  ; timeout_action : String.t option
  }

let make ~d_b_cluster_identifier ?capacity ?seconds_before_timeout ?timeout_action () =
  { d_b_cluster_identifier; capacity; seconds_before_timeout; timeout_action }

let parse xml =
  Some
    { d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; capacity = Aws.Util.option_bind (Aws.Xml.member "Capacity" xml) Integer.parse
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
       ; Aws.Util.option_map v.capacity (fun f ->
             Aws.Query.Pair ("Capacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timeout_action (fun f -> "TimeoutAction", String.to_json f)
       ; Aws.Util.option_map v.seconds_before_timeout (fun f ->
             "SecondsBeforeTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.capacity (fun f -> "Capacity", Integer.to_json f)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ])

let of_json j =
  { d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; capacity = Aws.Util.option_map (Aws.Json.lookup j "Capacity") Integer.of_json
  ; seconds_before_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "SecondsBeforeTimeout") Integer.of_json
  ; timeout_action =
      Aws.Util.option_map (Aws.Json.lookup j "TimeoutAction") String.of_json
  }
