open Aws.BaseTypes

type t =
  { scheduled_action_name : String.t
  ; error_code : String.t option
  ; error_message : String.t option
  }

let make ~scheduled_action_name ?error_code ?error_message () =
  { scheduled_action_name; error_code; error_message }

let parse xml =
  Some
    { scheduled_action_name =
        Aws.Xml.required
          "ScheduledActionName"
          (Aws.Util.option_bind (Aws.Xml.member "ScheduledActionName" xml) String.parse)
    ; error_code = Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ScheduledActionName", String.to_query v.scheduled_action_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_code (fun f -> "ErrorCode", String.to_json f)
       ; Some ("ScheduledActionName", String.to_json v.scheduled_action_name)
       ])

let of_json j =
  { scheduled_action_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionName"))
  ; error_code = Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  }
