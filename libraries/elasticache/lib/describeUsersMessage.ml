open Aws.BaseTypes

type t =
  { engine : String.t option
  ; user_id : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make ?engine ?user_id ?(filters = []) ?max_records ?marker () =
  { engine; user_id; filters; max_records; marker }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
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
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
