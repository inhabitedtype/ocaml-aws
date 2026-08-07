open Aws.BaseTypes

type t =
  { alarm_name : String.t option
  ; alarm_a_r_n : String.t option
  }

let make ?alarm_name ?alarm_a_r_n () = { alarm_name; alarm_a_r_n }

let parse xml =
  Some
    { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; alarm_a_r_n = Aws.Util.option_bind (Aws.Xml.member "AlarmARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alarm_a_r_n (fun f ->
             Aws.Query.Pair ("AlarmARN", String.to_query f))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alarm_a_r_n (fun f -> "AlarmARN", String.to_json f)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ])

let of_json j =
  { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; alarm_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "AlarmARN") String.of_json
  }
