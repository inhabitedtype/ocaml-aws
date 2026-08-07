open Aws.BaseTypes

type t =
  { events_matched : Long.t option
  ; events_scanned : Long.t option
  ; bytes_scanned : Long.t option
  ; execution_time_in_millis : Integer.t option
  ; creation_time : DateTime.t option
  }

let make
    ?events_matched
    ?events_scanned
    ?bytes_scanned
    ?execution_time_in_millis
    ?creation_time
    () =
  { events_matched
  ; events_scanned
  ; bytes_scanned
  ; execution_time_in_millis
  ; creation_time
  }

let parse xml =
  Some
    { events_matched =
        Aws.Util.option_bind (Aws.Xml.member "EventsMatched" xml) Long.parse
    ; events_scanned =
        Aws.Util.option_bind (Aws.Xml.member "EventsScanned" xml) Long.parse
    ; bytes_scanned = Aws.Util.option_bind (Aws.Xml.member "BytesScanned" xml) Long.parse
    ; execution_time_in_millis =
        Aws.Util.option_bind (Aws.Xml.member "ExecutionTimeInMillis" xml) Integer.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.execution_time_in_millis (fun f ->
             Aws.Query.Pair ("ExecutionTimeInMillis", Integer.to_query f))
       ; Aws.Util.option_map v.bytes_scanned (fun f ->
             Aws.Query.Pair ("BytesScanned", Long.to_query f))
       ; Aws.Util.option_map v.events_scanned (fun f ->
             Aws.Query.Pair ("EventsScanned", Long.to_query f))
       ; Aws.Util.option_map v.events_matched (fun f ->
             Aws.Query.Pair ("EventsMatched", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f -> "CreationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.execution_time_in_millis (fun f ->
             "ExecutionTimeInMillis", Integer.to_json f)
       ; Aws.Util.option_map v.bytes_scanned (fun f -> "BytesScanned", Long.to_json f)
       ; Aws.Util.option_map v.events_scanned (fun f -> "EventsScanned", Long.to_json f)
       ; Aws.Util.option_map v.events_matched (fun f -> "EventsMatched", Long.to_json f)
       ])

let of_json j =
  { events_matched = Aws.Util.option_map (Aws.Json.lookup j "EventsMatched") Long.of_json
  ; events_scanned = Aws.Util.option_map (Aws.Json.lookup j "EventsScanned") Long.of_json
  ; bytes_scanned = Aws.Util.option_map (Aws.Json.lookup j "BytesScanned") Long.of_json
  ; execution_time_in_millis =
      Aws.Util.option_map (Aws.Json.lookup j "ExecutionTimeInMillis") Integer.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
  }
