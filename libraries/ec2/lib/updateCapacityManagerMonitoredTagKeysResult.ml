type t = { capacity_manager_tag_keys : CapacityManagerMonitoredTagKeyList.t }

let make ?(capacity_manager_tag_keys = []) () = { capacity_manager_tag_keys }

let parse xml =
  Some
    { capacity_manager_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityManagerTagKeySet" xml)
             CapacityManagerMonitoredTagKeyList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityManagerTagKeySet"
              , CapacityManagerMonitoredTagKeyList.to_query v.capacity_manager_tag_keys ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityManagerTagKeySet"
           , CapacityManagerMonitoredTagKeyList.to_json v.capacity_manager_tag_keys )
       ])

let of_json j =
  { capacity_manager_tag_keys =
      CapacityManagerMonitoredTagKeyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityManagerTagKeySet"))
  }
