open Aws.BaseTypes

type t =
  { lookup_attributes : LookupAttributesList.t
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; event_category : EventCategory.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?(lookup_attributes = [])
    ?start_time
    ?end_time
    ?event_category
    ?max_results
    ?next_token
    () =
  { lookup_attributes; start_time; end_time; event_category; max_results; next_token }

let parse xml =
  Some
    { lookup_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LookupAttributes" xml)
             LookupAttributesList.parse)
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
    ; event_category =
        Aws.Util.option_bind (Aws.Xml.member "EventCategory" xml) EventCategory.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.event_category (fun f ->
             Aws.Query.Pair ("EventCategory", EventCategory.to_query f))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LookupAttributes.member"
              , LookupAttributesList.to_query v.lookup_attributes ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.event_category (fun f ->
             "EventCategory", EventCategory.to_json f)
       ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
       ; Some ("LookupAttributes", LookupAttributesList.to_json v.lookup_attributes)
       ])

let of_json j =
  { lookup_attributes =
      LookupAttributesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LookupAttributes"))
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
  ; event_category =
      Aws.Util.option_map (Aws.Json.lookup j "EventCategory") EventCategory.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
