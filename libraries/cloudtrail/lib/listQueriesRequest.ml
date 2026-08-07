open Aws.BaseTypes

type t =
  { event_data_store : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; query_status : QueryStatus.t option
  }

let make ~event_data_store ?next_token ?max_results ?start_time ?end_time ?query_status ()
    =
  { event_data_store; next_token; max_results; start_time; end_time; query_status }

let parse xml =
  Some
    { event_data_store =
        Aws.Xml.required
          "EventDataStore"
          (Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; query_status =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatus" xml) QueryStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.query_status (fun f ->
             Aws.Query.Pair ("QueryStatus", QueryStatus.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("EventDataStore", String.to_query v.event_data_store))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.query_status (fun f ->
             "QueryStatus", QueryStatus.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("EventDataStore", String.to_json v.event_data_store)
       ])

let of_json j =
  { event_data_store =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventDataStore"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; query_status =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatus") QueryStatus.of_json
  }
