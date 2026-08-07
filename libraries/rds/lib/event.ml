open Aws.BaseTypes

type t =
  { source_identifier : String.t option
  ; source_type : SourceType.t option
  ; message : String.t option
  ; event_categories : EventCategoriesList.t
  ; date : DateTime.t option
  ; source_arn : String.t option
  }

let make
    ?source_identifier
    ?source_type
    ?message
    ?(event_categories = [])
    ?date
    ?source_arn
    () =
  { source_identifier; source_type; message; event_categories; date; source_arn }

let parse xml =
  Some
    { source_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) SourceType.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             EventCategoriesList.parse)
    ; date = Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.date (fun f ->
             Aws.Query.Pair ("Date", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
       ; Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", SourceType.to_query f))
       ; Aws.Util.option_map v.source_identifier (fun f ->
             Aws.Query.Pair ("SourceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ; Aws.Util.option_map v.date (fun f -> "Date", DateTime.to_json f)
       ; Some ("EventCategories", EventCategoriesList.to_json v.event_categories)
       ; Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", SourceType.to_json f)
       ; Aws.Util.option_map v.source_identifier (fun f ->
             "SourceIdentifier", String.to_json f)
       ])

let of_json j =
  { source_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentifier") String.of_json
  ; source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") SourceType.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
  ; event_categories =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  ; date = Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  }
