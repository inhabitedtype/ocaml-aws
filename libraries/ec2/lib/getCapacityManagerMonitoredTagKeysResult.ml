open Aws.BaseTypes

type t =
  { capacity_manager_tag_keys : CapacityManagerMonitoredTagKeyList.t
  ; next_token : String.t option
  }

let make ?(capacity_manager_tag_keys = []) ?next_token () =
  { capacity_manager_tag_keys; next_token }

let parse xml =
  Some
    { capacity_manager_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityManagerTagKeySet" xml)
             CapacityManagerMonitoredTagKeyList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityManagerTagKeySet"
              , CapacityManagerMonitoredTagKeyList.to_query v.capacity_manager_tag_keys ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "capacityManagerTagKeySet"
           , CapacityManagerMonitoredTagKeyList.to_json v.capacity_manager_tag_keys )
       ])

let of_json j =
  { capacity_manager_tag_keys =
      CapacityManagerMonitoredTagKeyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityManagerTagKeySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
