open Aws.BaseTypes

type t =
  { rollback_triggers : RollbackTriggers.t
  ; monitoring_time_in_minutes : Integer.t option
  }

let make ?(rollback_triggers = []) ?monitoring_time_in_minutes () =
  { rollback_triggers; monitoring_time_in_minutes }

let parse xml =
  Some
    { rollback_triggers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RollbackTriggers" xml)
             RollbackTriggers.parse)
    ; monitoring_time_in_minutes =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringTimeInMinutes" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.monitoring_time_in_minutes (fun f ->
             Aws.Query.Pair ("MonitoringTimeInMinutes", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RollbackTriggers.member", RollbackTriggers.to_query v.rollback_triggers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.monitoring_time_in_minutes (fun f ->
             "MonitoringTimeInMinutes", Integer.to_json f)
       ; Some ("RollbackTriggers", RollbackTriggers.to_json v.rollback_triggers)
       ])

let of_json j =
  { rollback_triggers =
      RollbackTriggers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RollbackTriggers"))
  ; monitoring_time_in_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringTimeInMinutes") Integer.of_json
  }
