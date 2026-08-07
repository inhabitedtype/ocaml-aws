open Aws.BaseTypes

type t =
  { filter : FilterList.t
  ; max_duration : Integer.t option
  ; max_results : Integer.t option
  ; min_duration : Integer.t option
  ; next_token : String.t option
  ; offering_id : String.t option
  }

let make
    ?(filter = [])
    ?max_duration
    ?max_results
    ?min_duration
    ?next_token
    ?offering_id
    () =
  { filter; max_duration; max_results; min_duration; next_token; offering_id }

let parse xml =
  Some
    { filter =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_duration = Aws.Util.option_bind (Aws.Xml.member "MaxDuration" xml) Integer.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; min_duration = Aws.Util.option_bind (Aws.Xml.member "MinDuration" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; offering_id = Aws.Util.option_bind (Aws.Xml.member "OfferingId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_id (fun f ->
             Aws.Query.Pair ("OfferingId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.min_duration (fun f ->
             Aws.Query.Pair ("MinDuration", Integer.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.max_duration (fun f ->
             Aws.Query.Pair ("MaxDuration", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filter))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_id (fun f -> "OfferingId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.min_duration (fun f -> "MinDuration", Integer.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.max_duration (fun f -> "MaxDuration", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filter)
       ])

let of_json j =
  { filter = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_duration = Aws.Util.option_map (Aws.Json.lookup j "MaxDuration") Integer.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; min_duration = Aws.Util.option_map (Aws.Json.lookup j "MinDuration") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; offering_id = Aws.Util.option_map (Aws.Json.lookup j "OfferingId") String.of_json
  }
