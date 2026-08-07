open Aws.BaseTypes

type t =
  { query : String.t
  ; pending_period : Integer.t option
  ; recovery_period : Integer.t option
  }

let make ~query ?pending_period ?recovery_period () =
  { query; pending_period; recovery_period }

let parse xml =
  Some
    { query =
        Aws.Xml.required
          "Query"
          (Aws.Util.option_bind (Aws.Xml.member "Query" xml) String.parse)
    ; pending_period =
        Aws.Util.option_bind (Aws.Xml.member "PendingPeriod" xml) Integer.parse
    ; recovery_period =
        Aws.Util.option_bind (Aws.Xml.member "RecoveryPeriod" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recovery_period (fun f ->
             Aws.Query.Pair ("RecoveryPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.pending_period (fun f ->
             Aws.Query.Pair ("PendingPeriod", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Query", String.to_query v.query))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recovery_period (fun f ->
             "RecoveryPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.pending_period (fun f ->
             "PendingPeriod", Integer.to_json f)
       ; Some ("Query", String.to_json v.query)
       ])

let of_json j =
  { query = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Query"))
  ; pending_period =
      Aws.Util.option_map (Aws.Json.lookup j "PendingPeriod") Integer.of_json
  ; recovery_period =
      Aws.Util.option_map (Aws.Json.lookup j "RecoveryPeriod") Integer.of_json
  }
