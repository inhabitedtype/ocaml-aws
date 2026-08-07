open Aws.BaseTypes

type t =
  { source_identifier : String.t option
  ; source_type : SourceType.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; duration : Integer.t option
  ; event_categories : EventCategoriesList.t
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?source_identifier
    ?source_type
    ?start_time
    ?end_time
    ?duration
    ?(event_categories = [])
    ?(filters = [])
    ?max_records
    ?marker
    () =
  { source_identifier
  ; source_type
  ; start_time
  ; end_time
  ; duration
  ; event_categories
  ; filters
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { source_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) SourceType.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) Integer.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             EventCategoriesList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Integer.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", SourceType.to_query f))
       ; Aws.Util.option_map v.source_identifier (fun f ->
             Aws.Query.Pair ("SourceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Some ("EventCategories", EventCategoriesList.to_json v.event_categories)
       ; Aws.Util.option_map v.duration (fun f -> "Duration", Integer.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", SourceType.to_json f)
       ; Aws.Util.option_map v.source_identifier (fun f ->
             "SourceIdentifier", String.to_json f)
       ])

let of_json j =
  { source_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentifier") String.of_json
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") SourceType.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") Integer.of_json
  ; event_categories =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
