open Aws.BaseTypes

type t =
  { stack_drift_status : StackDriftStatus.t
  ; last_check_timestamp : DateTime.t option
  }

let make ~stack_drift_status ?last_check_timestamp () =
  { stack_drift_status; last_check_timestamp }

let parse xml =
  Some
    { stack_drift_status =
        Aws.Xml.required
          "StackDriftStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackDriftStatus" xml)
             StackDriftStatus.parse)
    ; last_check_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastCheckTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_check_timestamp (fun f ->
             Aws.Query.Pair ("LastCheckTimestamp", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ("StackDriftStatus", StackDriftStatus.to_query v.stack_drift_status))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_check_timestamp (fun f ->
             "LastCheckTimestamp", DateTime.to_json f)
       ; Some ("StackDriftStatus", StackDriftStatus.to_json v.stack_drift_status)
       ])

let of_json j =
  { stack_drift_status =
      StackDriftStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftStatus"))
  ; last_check_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
  }
