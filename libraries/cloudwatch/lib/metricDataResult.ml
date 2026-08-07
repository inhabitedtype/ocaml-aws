open Aws.BaseTypes

type t =
  { id : String.t option
  ; label : String.t option
  ; timestamps : Timestamps.t
  ; values : DatapointValues.t
  ; status_code : StatusCode.t option
  ; messages : MetricDataResultMessages.t
  }

let make ?id ?label ?(timestamps = []) ?(values = []) ?status_code ?(messages = []) () =
  { id; label; timestamps; values; status_code; messages }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
    ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
    ; timestamps =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Timestamps" xml) Timestamps.parse)
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Values" xml) DatapointValues.parse)
    ; status_code =
        Aws.Util.option_bind (Aws.Xml.member "StatusCode" xml) StatusCode.parse
    ; messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Messages" xml)
             MetricDataResultMessages.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Messages.member", MetricDataResultMessages.to_query v.messages))
       ; Aws.Util.option_map v.status_code (fun f ->
             Aws.Query.Pair ("StatusCode", StatusCode.to_query f))
       ; Some (Aws.Query.Pair ("Values.member", DatapointValues.to_query v.values))
       ; Some (Aws.Query.Pair ("Timestamps.member", Timestamps.to_query v.timestamps))
       ; Aws.Util.option_map v.label (fun f ->
             Aws.Query.Pair ("Label", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Messages", MetricDataResultMessages.to_json v.messages)
       ; Aws.Util.option_map v.status_code (fun f -> "StatusCode", StatusCode.to_json f)
       ; Some ("Values", DatapointValues.to_json v.values)
       ; Some ("Timestamps", Timestamps.to_json v.timestamps)
       ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
  ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
  ; timestamps =
      Timestamps.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamps"))
  ; values = DatapointValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  ; status_code = Aws.Util.option_map (Aws.Json.lookup j "StatusCode") StatusCode.of_json
  ; messages =
      MetricDataResultMessages.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
  }
