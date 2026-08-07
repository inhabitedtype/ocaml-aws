open Aws.BaseTypes

type t =
  { alarm_history_items : AlarmHistoryItems.t
  ; next_token : String.t option
  }

let make ?(alarm_history_items = []) ?next_token () = { alarm_history_items; next_token }

let parse xml =
  Some
    { alarm_history_items =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AlarmHistoryItems" xml)
             AlarmHistoryItems.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AlarmHistoryItems.member"
              , AlarmHistoryItems.to_query v.alarm_history_items ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("AlarmHistoryItems", AlarmHistoryItems.to_json v.alarm_history_items)
       ])

let of_json j =
  { alarm_history_items =
      AlarmHistoryItems.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmHistoryItems"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
