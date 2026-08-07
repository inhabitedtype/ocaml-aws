open Aws.BaseTypes

type t =
  { event_information : EventInformation.t option
  ; event_type : FleetEventType.t option
  ; timestamp : DateTime.t option
  }

let make ?event_information ?event_type ?timestamp () =
  { event_information; event_type; timestamp }

let parse xml =
  Some
    { event_information =
        Aws.Util.option_bind
          (Aws.Xml.member "eventInformation" xml)
          EventInformation.parse
    ; event_type =
        Aws.Util.option_bind (Aws.Xml.member "eventType" xml) FleetEventType.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.event_type (fun f ->
             Aws.Query.Pair ("EventType", FleetEventType.to_query f))
       ; Aws.Util.option_map v.event_information (fun f ->
             Aws.Query.Pair ("EventInformation", EventInformation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.event_type (fun f -> "eventType", FleetEventType.to_json f)
       ; Aws.Util.option_map v.event_information (fun f ->
             "eventInformation", EventInformation.to_json f)
       ])

let of_json j =
  { event_information =
      Aws.Util.option_map (Aws.Json.lookup j "eventInformation") EventInformation.of_json
  ; event_type =
      Aws.Util.option_map (Aws.Json.lookup j "eventType") FleetEventType.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") DateTime.of_json
  }
