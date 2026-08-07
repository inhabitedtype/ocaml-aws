open Aws.BaseTypes

type t =
  { source_type : String.t option
  ; event_categories : EventCategoriesList.t
  }

let make ?source_type ?(event_categories = []) () = { source_type; event_categories }

let parse xml =
  Some
    { source_type = Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) String.parse
    ; event_categories =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategories" xml)
             EventCategoriesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("EventCategories.member", EventCategoriesList.to_query v.event_categories))
       ; Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("EventCategories", EventCategoriesList.to_json v.event_categories)
       ; Aws.Util.option_map v.source_type (fun f -> "SourceType", String.to_json f)
       ])

let of_json j =
  { source_type = Aws.Util.option_map (Aws.Json.lookup j "SourceType") String.of_json
  ; event_categories =
      EventCategoriesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategories"))
  }
