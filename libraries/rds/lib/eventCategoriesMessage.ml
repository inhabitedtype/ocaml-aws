type t = { event_categories_map_list : EventCategoriesMapList.t }

let make ?(event_categories_map_list = []) () = { event_categories_map_list }

let parse xml =
  Some
    { event_categories_map_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EventCategoriesMapList" xml)
             EventCategoriesMapList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "EventCategoriesMapList.member"
              , EventCategoriesMapList.to_query v.event_categories_map_list ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "EventCategoriesMapList"
           , EventCategoriesMapList.to_json v.event_categories_map_list )
       ])

let of_json j =
  { event_categories_map_list =
      EventCategoriesMapList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EventCategoriesMapList"))
  }
