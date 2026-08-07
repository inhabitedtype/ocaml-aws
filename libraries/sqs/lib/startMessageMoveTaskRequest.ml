open Aws.BaseTypes

type t =
  { source_arn : String.t
  ; destination_arn : String.t option
  ; max_number_of_messages_per_second : Integer.t option
  }

let make ~source_arn ?destination_arn ?max_number_of_messages_per_second () =
  { source_arn; destination_arn; max_number_of_messages_per_second }

let parse xml =
  Some
    { source_arn =
        Aws.Xml.required
          "SourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse)
    ; destination_arn =
        Aws.Util.option_bind (Aws.Xml.member "DestinationArn" xml) String.parse
    ; max_number_of_messages_per_second =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxNumberOfMessagesPerSecond" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_number_of_messages_per_second (fun f ->
             Aws.Query.Pair ("MaxNumberOfMessagesPerSecond", Integer.to_query f))
       ; Aws.Util.option_map v.destination_arn (fun f ->
             Aws.Query.Pair ("DestinationArn", String.to_query f))
       ; Some (Aws.Query.Pair ("SourceArn", String.to_query v.source_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_number_of_messages_per_second (fun f ->
             "MaxNumberOfMessagesPerSecond", Integer.to_json f)
       ; Aws.Util.option_map v.destination_arn (fun f ->
             "DestinationArn", String.to_json f)
       ; Some ("SourceArn", String.to_json v.source_arn)
       ])

let of_json j =
  { source_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceArn"))
  ; destination_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationArn") String.of_json
  ; max_number_of_messages_per_second =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxNumberOfMessagesPerSecond")
        Integer.of_json
  }
