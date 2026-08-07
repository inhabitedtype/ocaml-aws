type t =
  { insight_type : InsightType.t option
  ; event_categories : SourceEventCategories.t
  }

let make ?insight_type ?(event_categories = []) () = { insight_type; event_categories }

let parse xml =
  Some
    { insight_type =
        Aws.Util.option_bind (Aws.Xml.member "InsightType" xml) InsightType.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             SourceEventCategories.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("EventCategories.member", SourceEventCategories.to_query v.event_categories))
       ; Aws.Util.option_map v.insight_type (fun f ->
             Aws.Query.Pair ("InsightType", InsightType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EventCategories", SourceEventCategories.to_json v.event_categories)
       ; Aws.Util.option_map v.insight_type (fun f ->
             "InsightType", InsightType.to_json f)
       ])

let of_json j =
  { insight_type =
      Aws.Util.option_map (Aws.Json.lookup j "InsightType") InsightType.of_json
  ; event_categories =
      SourceEventCategories.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  }
