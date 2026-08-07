open Aws.BaseTypes

type t =
  { arn : String.t option
  ; creation_date : DateTime.t option
  ; last_update_date : DateTime.t option
  ; name : String.t option
  ; firehose_arn : String.t option
  ; state : String.t option
  ; output_format : MetricStreamOutputFormat.t option
  }

let make
    ?arn
    ?creation_date
    ?last_update_date
    ?name
    ?firehose_arn
    ?state
    ?output_format
    () =
  { arn; creation_date; last_update_date; name; firehose_arn; state; output_format }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
    ; last_update_date =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdateDate" xml) DateTime.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; firehose_arn = Aws.Util.option_bind (Aws.Xml.member "FirehoseArn" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    ; output_format =
        Aws.Util.option_bind
          (Aws.Xml.member "OutputFormat" xml)
          MetricStreamOutputFormat.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output_format (fun f ->
             Aws.Query.Pair ("OutputFormat", MetricStreamOutputFormat.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.firehose_arn (fun f ->
             Aws.Query.Pair ("FirehoseArn", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.last_update_date (fun f ->
             Aws.Query.Pair ("LastUpdateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output_format (fun f ->
             "OutputFormat", MetricStreamOutputFormat.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.firehose_arn (fun f -> "FirehoseArn", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.last_update_date (fun f ->
             "LastUpdateDate", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
  ; last_update_date =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdateDate") DateTime.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; firehose_arn = Aws.Util.option_map (Aws.Json.lookup j "FirehoseArn") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  ; output_format =
      Aws.Util.option_map
        (Aws.Json.lookup j "OutputFormat")
        MetricStreamOutputFormat.of_json
  }
