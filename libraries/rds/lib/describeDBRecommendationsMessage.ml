open Aws.BaseTypes

type t =
  { last_updated_after : DateTime.t option
  ; last_updated_before : DateTime.t option
  ; locale : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?last_updated_after
    ?last_updated_before
    ?locale
    ?(filters = [])
    ?max_records
    ?marker
    () =
  { last_updated_after; last_updated_before; locale; filters; max_records; marker }

let parse xml =
  Some
    { last_updated_after =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedAfter" xml) DateTime.parse
    ; last_updated_before =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedBefore" xml) DateTime.parse
    ; locale = Aws.Util.option_bind (Aws.Xml.member "Locale" xml) String.parse
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
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Aws.Util.option_map v.last_updated_before (fun f ->
             Aws.Query.Pair ("LastUpdatedBefore", DateTime.to_query f))
       ; Aws.Util.option_map v.last_updated_after (fun f ->
             Aws.Query.Pair ("LastUpdatedAfter", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.locale (fun f -> "Locale", String.to_json f)
       ; Aws.Util.option_map v.last_updated_before (fun f ->
             "LastUpdatedBefore", DateTime.to_json f)
       ; Aws.Util.option_map v.last_updated_after (fun f ->
             "LastUpdatedAfter", DateTime.to_json f)
       ])

let of_json j =
  { last_updated_after =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedAfter") DateTime.of_json
  ; last_updated_before =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedBefore") DateTime.of_json
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "Locale") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
