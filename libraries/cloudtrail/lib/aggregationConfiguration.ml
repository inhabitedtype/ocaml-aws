type t =
  { templates : Templates.t
  ; event_category : EventCategoryAggregation.t
  }

let make ~templates ~event_category () = { templates; event_category }

let parse xml =
  Some
    { templates =
        Aws.Xml.required
          "Templates"
          (Aws.Util.option_bind (Aws.Xml.member "Templates" xml) Templates.parse)
    ; event_category =
        Aws.Xml.required
          "EventCategory"
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategory" xml)
             EventCategoryAggregation.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("EventCategory", EventCategoryAggregation.to_query v.event_category))
       ; Some (Aws.Query.Pair ("Templates.member", Templates.to_query v.templates))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EventCategory", EventCategoryAggregation.to_json v.event_category)
       ; Some ("Templates", Templates.to_json v.templates)
       ])

let of_json j =
  { templates = Templates.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Templates"))
  ; event_category =
      EventCategoryAggregation.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategory"))
  }
