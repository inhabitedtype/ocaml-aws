open Aws.BaseTypes

type t =
  { history_records : HistoryRecords.t
  ; last_evaluated_time : DateTime.t option
  ; next_token : String.t option
  ; spot_fleet_request_id : String.t option
  ; start_time : DateTime.t option
  }

let make
    ?(history_records = [])
    ?last_evaluated_time
    ?next_token
    ?spot_fleet_request_id
    ?start_time
    () =
  { history_records; last_evaluated_time; next_token; spot_fleet_request_id; start_time }

let parse xml =
  Some
    { history_records =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "historyRecordSet" xml)
             HistoryRecords.parse)
    ; last_evaluated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastEvaluatedTime" xml) DateTime.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; spot_fleet_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             Aws.Query.Pair ("SpotFleetRequestId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.last_evaluated_time (fun f ->
             Aws.Query.Pair ("LastEvaluatedTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair ("HistoryRecordSet", HistoryRecords.to_query v.history_records))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.spot_fleet_request_id (fun f ->
             "spotFleetRequestId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.last_evaluated_time (fun f ->
             "lastEvaluatedTime", DateTime.to_json f)
       ; Some ("historyRecordSet", HistoryRecords.to_json v.history_records)
       ])

let of_json j =
  { history_records =
      HistoryRecords.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "historyRecordSet"))
  ; last_evaluated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastEvaluatedTime") DateTime.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; spot_fleet_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotFleetRequestId") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  }
