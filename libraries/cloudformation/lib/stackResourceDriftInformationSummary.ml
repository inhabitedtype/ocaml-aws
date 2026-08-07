open Aws.BaseTypes

type t =
  { stack_resource_drift_status : StackResourceDriftStatus.t
  ; last_check_timestamp : DateTime.t option
  }

let make ~stack_resource_drift_status ?last_check_timestamp () =
  { stack_resource_drift_status; last_check_timestamp }

let parse xml =
  Some
    { stack_resource_drift_status =
        Aws.Xml.required
          "StackResourceDriftStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceDriftStatus" xml)
             StackResourceDriftStatus.parse)
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
              ( "StackResourceDriftStatus"
              , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_check_timestamp (fun f ->
             "LastCheckTimestamp", DateTime.to_json f)
       ; Some
           ( "StackResourceDriftStatus"
           , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
       ])

let of_json j =
  { stack_resource_drift_status =
      StackResourceDriftStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
  ; last_check_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
  }
