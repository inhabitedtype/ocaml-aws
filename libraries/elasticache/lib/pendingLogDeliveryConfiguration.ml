type t =
  { log_type : LogType.t option
  ; destination_type : DestinationType.t option
  ; destination_details : DestinationDetails.t option
  ; log_format : LogFormat.t option
  }

let make ?log_type ?destination_type ?destination_details ?log_format () =
  { log_type; destination_type; destination_details; log_format }

let parse xml =
  Some
    { log_type = Aws.Util.option_bind (Aws.Xml.member "LogType" xml) LogType.parse
    ; destination_type =
        Aws.Util.option_bind (Aws.Xml.member "DestinationType" xml) DestinationType.parse
    ; destination_details =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationDetails" xml)
          DestinationDetails.parse
    ; log_format = Aws.Util.option_bind (Aws.Xml.member "LogFormat" xml) LogFormat.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_format (fun f ->
             Aws.Query.Pair ("LogFormat", LogFormat.to_query f))
       ; Aws.Util.option_map v.destination_details (fun f ->
             Aws.Query.Pair ("DestinationDetails", DestinationDetails.to_query f))
       ; Aws.Util.option_map v.destination_type (fun f ->
             Aws.Query.Pair ("DestinationType", DestinationType.to_query f))
       ; Aws.Util.option_map v.log_type (fun f ->
             Aws.Query.Pair ("LogType", LogType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_format (fun f -> "LogFormat", LogFormat.to_json f)
       ; Aws.Util.option_map v.destination_details (fun f ->
             "DestinationDetails", DestinationDetails.to_json f)
       ; Aws.Util.option_map v.destination_type (fun f ->
             "DestinationType", DestinationType.to_json f)
       ; Aws.Util.option_map v.log_type (fun f -> "LogType", LogType.to_json f)
       ])

let of_json j =
  { log_type = Aws.Util.option_map (Aws.Json.lookup j "LogType") LogType.of_json
  ; destination_type =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationType") DestinationType.of_json
  ; destination_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationDetails")
        DestinationDetails.of_json
  ; log_format = Aws.Util.option_map (Aws.Json.lookup j "LogFormat") LogFormat.of_json
  }
