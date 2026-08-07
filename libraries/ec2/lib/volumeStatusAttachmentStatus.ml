open Aws.BaseTypes

type t =
  { io_performance : String.t option
  ; instance_id : String.t option
  }

let make ?io_performance ?instance_id () = { io_performance; instance_id }

let parse xml =
  Some
    { io_performance =
        Aws.Util.option_bind (Aws.Xml.member "ioPerformance" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.io_performance (fun f ->
             Aws.Query.Pair ("IoPerformance", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.io_performance (fun f -> "ioPerformance", String.to_json f)
       ])

let of_json j =
  { io_performance =
      Aws.Util.option_map (Aws.Json.lookup j "ioPerformance") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  }
