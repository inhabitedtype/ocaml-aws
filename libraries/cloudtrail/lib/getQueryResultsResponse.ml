open Aws.BaseTypes

type t =
  { query_status : QueryStatus.t option
  ; query_statistics : QueryStatistics.t option
  ; query_result_rows : QueryResultRows.t
  ; next_token : String.t option
  ; error_message : String.t option
  }

let make
    ?query_status
    ?query_statistics
    ?(query_result_rows = [])
    ?next_token
    ?error_message
    () =
  { query_status; query_statistics; query_result_rows; next_token; error_message }

let parse xml =
  Some
    { query_status =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatus" xml) QueryStatus.parse
    ; query_statistics =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatistics" xml) QueryStatistics.parse
    ; query_result_rows =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryResultRows" xml)
             QueryResultRows.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("QueryResultRows.member", QueryResultRows.to_query v.query_result_rows))
       ; Aws.Util.option_map v.query_statistics (fun f ->
             Aws.Query.Pair ("QueryStatistics", QueryStatistics.to_query f))
       ; Aws.Util.option_map v.query_status (fun f ->
             Aws.Query.Pair ("QueryStatus", QueryStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("QueryResultRows", QueryResultRows.to_json v.query_result_rows)
       ; Aws.Util.option_map v.query_statistics (fun f ->
             "QueryStatistics", QueryStatistics.to_json f)
       ; Aws.Util.option_map v.query_status (fun f ->
             "QueryStatus", QueryStatus.to_json f)
       ])

let of_json j =
  { query_status =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatus") QueryStatus.of_json
  ; query_statistics =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatistics") QueryStatistics.of_json
  ; query_result_rows =
      QueryResultRows.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryResultRows"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  }
