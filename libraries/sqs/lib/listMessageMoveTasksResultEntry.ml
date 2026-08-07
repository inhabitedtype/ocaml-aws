open Aws.BaseTypes

type t =
  { task_handle : String.t option
  ; status : String.t option
  ; source_arn : String.t option
  ; destination_arn : String.t option
  ; max_number_of_messages_per_second : Integer.t option
  ; approximate_number_of_messages_moved : Long.t option
  ; approximate_number_of_messages_to_move : Long.t option
  ; failure_reason : String.t option
  ; started_timestamp : Long.t option
  }

let make
    ?task_handle
    ?status
    ?source_arn
    ?destination_arn
    ?max_number_of_messages_per_second
    ?approximate_number_of_messages_moved
    ?approximate_number_of_messages_to_move
    ?failure_reason
    ?started_timestamp
    () =
  { task_handle
  ; status
  ; source_arn
  ; destination_arn
  ; max_number_of_messages_per_second
  ; approximate_number_of_messages_moved
  ; approximate_number_of_messages_to_move
  ; failure_reason
  ; started_timestamp
  }

let parse xml =
  Some
    { task_handle = Aws.Util.option_bind (Aws.Xml.member "TaskHandle" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    ; destination_arn =
        Aws.Util.option_bind (Aws.Xml.member "DestinationArn" xml) String.parse
    ; max_number_of_messages_per_second =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxNumberOfMessagesPerSecond" xml)
          Integer.parse
    ; approximate_number_of_messages_moved =
        Aws.Util.option_bind
          (Aws.Xml.member "ApproximateNumberOfMessagesMoved" xml)
          Long.parse
    ; approximate_number_of_messages_to_move =
        Aws.Util.option_bind
          (Aws.Xml.member "ApproximateNumberOfMessagesToMove" xml)
          Long.parse
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "FailureReason" xml) String.parse
    ; started_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "StartedTimestamp" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.started_timestamp (fun f ->
             Aws.Query.Pair ("StartedTimestamp", Long.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Aws.Util.option_map v.approximate_number_of_messages_to_move (fun f ->
             Aws.Query.Pair ("ApproximateNumberOfMessagesToMove", Long.to_query f))
       ; Aws.Util.option_map v.approximate_number_of_messages_moved (fun f ->
             Aws.Query.Pair ("ApproximateNumberOfMessagesMoved", Long.to_query f))
       ; Aws.Util.option_map v.max_number_of_messages_per_second (fun f ->
             Aws.Query.Pair ("MaxNumberOfMessagesPerSecond", Integer.to_query f))
       ; Aws.Util.option_map v.destination_arn (fun f ->
             Aws.Query.Pair ("DestinationArn", String.to_query f))
       ; Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.task_handle (fun f ->
             Aws.Query.Pair ("TaskHandle", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.started_timestamp (fun f ->
             "StartedTimestamp", Long.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f -> "FailureReason", String.to_json f)
       ; Aws.Util.option_map v.approximate_number_of_messages_to_move (fun f ->
             "ApproximateNumberOfMessagesToMove", Long.to_json f)
       ; Aws.Util.option_map v.approximate_number_of_messages_moved (fun f ->
             "ApproximateNumberOfMessagesMoved", Long.to_json f)
       ; Aws.Util.option_map v.max_number_of_messages_per_second (fun f ->
             "MaxNumberOfMessagesPerSecond", Integer.to_json f)
       ; Aws.Util.option_map v.destination_arn (fun f ->
             "DestinationArn", String.to_json f)
       ; Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.task_handle (fun f -> "TaskHandle", String.to_json f)
       ])

let of_json j =
  { task_handle = Aws.Util.option_map (Aws.Json.lookup j "TaskHandle") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  ; destination_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationArn") String.of_json
  ; max_number_of_messages_per_second =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxNumberOfMessagesPerSecond")
        Integer.of_json
  ; approximate_number_of_messages_moved =
      Aws.Util.option_map
        (Aws.Json.lookup j "ApproximateNumberOfMessagesMoved")
        Long.of_json
  ; approximate_number_of_messages_to_move =
      Aws.Util.option_map
        (Aws.Json.lookup j "ApproximateNumberOfMessagesToMove")
        Long.of_json
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "FailureReason") String.of_json
  ; started_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "StartedTimestamp") Long.of_json
  }
