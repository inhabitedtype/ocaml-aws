open Aws.BaseTypes

type t =
  { import_id : String.t option
  ; import_status : ImportStatus.t option
  ; destinations : ImportDestinations.t
  ; created_timestamp : DateTime.t option
  ; updated_timestamp : DateTime.t option
  }

let make
    ?import_id
    ?import_status
    ?(destinations = [])
    ?created_timestamp
    ?updated_timestamp
    () =
  { import_id; import_status; destinations; created_timestamp; updated_timestamp }

let parse xml =
  Some
    { import_id = Aws.Util.option_bind (Aws.Xml.member "ImportId" xml) String.parse
    ; import_status =
        Aws.Util.option_bind (Aws.Xml.member "ImportStatus" xml) ImportStatus.parse
    ; destinations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Destinations" xml)
             ImportDestinations.parse)
    ; created_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "CreatedTimestamp" xml) DateTime.parse
    ; updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "UpdatedTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_timestamp (fun f ->
             Aws.Query.Pair ("UpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.created_timestamp (fun f ->
             Aws.Query.Pair ("CreatedTimestamp", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Destinations.member", ImportDestinations.to_query v.destinations))
       ; Aws.Util.option_map v.import_status (fun f ->
             Aws.Query.Pair ("ImportStatus", ImportStatus.to_query f))
       ; Aws.Util.option_map v.import_id (fun f ->
             Aws.Query.Pair ("ImportId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.updated_timestamp (fun f ->
             "UpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.created_timestamp (fun f ->
             "CreatedTimestamp", DateTime.to_json f)
       ; Some ("Destinations", ImportDestinations.to_json v.destinations)
       ; Aws.Util.option_map v.import_status (fun f ->
             "ImportStatus", ImportStatus.to_json f)
       ; Aws.Util.option_map v.import_id (fun f -> "ImportId", String.to_json f)
       ])

let of_json j =
  { import_id = Aws.Util.option_map (Aws.Json.lookup j "ImportId") String.of_json
  ; import_status =
      Aws.Util.option_map (Aws.Json.lookup j "ImportStatus") ImportStatus.of_json
  ; destinations =
      ImportDestinations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Destinations"))
  ; created_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "CreatedTimestamp") DateTime.of_json
  ; updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "UpdatedTimestamp") DateTime.of_json
  }
