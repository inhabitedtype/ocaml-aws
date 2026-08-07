open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; cloudwatch_log_group : String.t option
  ; cloudwatch_log_stream : String.t option
  }

let make ?enabled ?cloudwatch_log_group ?cloudwatch_log_stream () =
  { enabled; cloudwatch_log_group; cloudwatch_log_stream }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; cloudwatch_log_group =
        Aws.Util.option_bind (Aws.Xml.member "CloudwatchLogGroup" xml) String.parse
    ; cloudwatch_log_stream =
        Aws.Util.option_bind (Aws.Xml.member "CloudwatchLogStream" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloudwatch_log_stream (fun f ->
             Aws.Query.Pair ("CloudwatchLogStream", String.to_query f))
       ; Aws.Util.option_map v.cloudwatch_log_group (fun f ->
             Aws.Query.Pair ("CloudwatchLogGroup", String.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cloudwatch_log_stream (fun f ->
             "CloudwatchLogStream", String.to_json f)
       ; Aws.Util.option_map v.cloudwatch_log_group (fun f ->
             "CloudwatchLogGroup", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; cloudwatch_log_group =
      Aws.Util.option_map (Aws.Json.lookup j "CloudwatchLogGroup") String.of_json
  ; cloudwatch_log_stream =
      Aws.Util.option_map (Aws.Json.lookup j "CloudwatchLogStream") String.of_json
  }
