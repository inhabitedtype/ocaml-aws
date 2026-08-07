open Aws.BaseTypes

type t =
  { destinations : ImportDestinations.t
  ; import_source : ImportSource.t option
  ; start_event_time : DateTime.t option
  ; end_event_time : DateTime.t option
  ; import_id : String.t option
  }

let make
    ?(destinations = [])
    ?import_source
    ?start_event_time
    ?end_event_time
    ?import_id
    () =
  { destinations; import_source; start_event_time; end_event_time; import_id }

let parse xml =
  Some
    { destinations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Destinations" xml)
             ImportDestinations.parse)
    ; import_source =
        Aws.Util.option_bind (Aws.Xml.member "ImportSource" xml) ImportSource.parse
    ; start_event_time =
        Aws.Util.option_bind (Aws.Xml.member "StartEventTime" xml) DateTime.parse
    ; end_event_time =
        Aws.Util.option_bind (Aws.Xml.member "EndEventTime" xml) DateTime.parse
    ; import_id = Aws.Util.option_bind (Aws.Xml.member "ImportId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_id (fun f ->
             Aws.Query.Pair ("ImportId", String.to_query f))
       ; Aws.Util.option_map v.end_event_time (fun f ->
             Aws.Query.Pair ("EndEventTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_event_time (fun f ->
             Aws.Query.Pair ("StartEventTime", DateTime.to_query f))
       ; Aws.Util.option_map v.import_source (fun f ->
             Aws.Query.Pair ("ImportSource", ImportSource.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Destinations.member", ImportDestinations.to_query v.destinations))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.import_id (fun f -> "ImportId", String.to_json f)
       ; Aws.Util.option_map v.end_event_time (fun f ->
             "EndEventTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_event_time (fun f ->
             "StartEventTime", DateTime.to_json f)
       ; Aws.Util.option_map v.import_source (fun f ->
             "ImportSource", ImportSource.to_json f)
       ; Some ("Destinations", ImportDestinations.to_json v.destinations)
       ])

let of_json j =
  { destinations =
      ImportDestinations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Destinations"))
  ; import_source =
      Aws.Util.option_map (Aws.Json.lookup j "ImportSource") ImportSource.of_json
  ; start_event_time =
      Aws.Util.option_map (Aws.Json.lookup j "StartEventTime") DateTime.of_json
  ; end_event_time =
      Aws.Util.option_map (Aws.Json.lookup j "EndEventTime") DateTime.of_json
  ; import_id = Aws.Util.option_map (Aws.Json.lookup j "ImportId") String.of_json
  }
