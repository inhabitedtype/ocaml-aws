open Aws.BaseTypes

type t =
  { latest_ingestion_success_time : DateTime.t option
  ; latest_ingestion_success_event_i_d : String.t option
  ; latest_ingestion_error_code : String.t option
  ; latest_ingestion_attempt_time : DateTime.t option
  ; latest_ingestion_attempt_event_i_d : String.t option
  }

let make
    ?latest_ingestion_success_time
    ?latest_ingestion_success_event_i_d
    ?latest_ingestion_error_code
    ?latest_ingestion_attempt_time
    ?latest_ingestion_attempt_event_i_d
    () =
  { latest_ingestion_success_time
  ; latest_ingestion_success_event_i_d
  ; latest_ingestion_error_code
  ; latest_ingestion_attempt_time
  ; latest_ingestion_attempt_event_i_d
  }

let parse xml =
  Some
    { latest_ingestion_success_time =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestIngestionSuccessTime" xml)
          DateTime.parse
    ; latest_ingestion_success_event_i_d =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestIngestionSuccessEventID" xml)
          String.parse
    ; latest_ingestion_error_code =
        Aws.Util.option_bind (Aws.Xml.member "LatestIngestionErrorCode" xml) String.parse
    ; latest_ingestion_attempt_time =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestIngestionAttemptTime" xml)
          DateTime.parse
    ; latest_ingestion_attempt_event_i_d =
        Aws.Util.option_bind
          (Aws.Xml.member "LatestIngestionAttemptEventID" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_ingestion_attempt_event_i_d (fun f ->
             Aws.Query.Pair ("LatestIngestionAttemptEventID", String.to_query f))
       ; Aws.Util.option_map v.latest_ingestion_attempt_time (fun f ->
             Aws.Query.Pair ("LatestIngestionAttemptTime", DateTime.to_query f))
       ; Aws.Util.option_map v.latest_ingestion_error_code (fun f ->
             Aws.Query.Pair ("LatestIngestionErrorCode", String.to_query f))
       ; Aws.Util.option_map v.latest_ingestion_success_event_i_d (fun f ->
             Aws.Query.Pair ("LatestIngestionSuccessEventID", String.to_query f))
       ; Aws.Util.option_map v.latest_ingestion_success_time (fun f ->
             Aws.Query.Pair ("LatestIngestionSuccessTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.latest_ingestion_attempt_event_i_d (fun f ->
             "LatestIngestionAttemptEventID", String.to_json f)
       ; Aws.Util.option_map v.latest_ingestion_attempt_time (fun f ->
             "LatestIngestionAttemptTime", DateTime.to_json f)
       ; Aws.Util.option_map v.latest_ingestion_error_code (fun f ->
             "LatestIngestionErrorCode", String.to_json f)
       ; Aws.Util.option_map v.latest_ingestion_success_event_i_d (fun f ->
             "LatestIngestionSuccessEventID", String.to_json f)
       ; Aws.Util.option_map v.latest_ingestion_success_time (fun f ->
             "LatestIngestionSuccessTime", DateTime.to_json f)
       ])

let of_json j =
  { latest_ingestion_success_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestIngestionSuccessTime")
        DateTime.of_json
  ; latest_ingestion_success_event_i_d =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestIngestionSuccessEventID")
        String.of_json
  ; latest_ingestion_error_code =
      Aws.Util.option_map (Aws.Json.lookup j "LatestIngestionErrorCode") String.of_json
  ; latest_ingestion_attempt_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestIngestionAttemptTime")
        DateTime.of_json
  ; latest_ingestion_attempt_event_i_d =
      Aws.Util.option_map
        (Aws.Json.lookup j "LatestIngestionAttemptEventID")
        String.of_json
  }
