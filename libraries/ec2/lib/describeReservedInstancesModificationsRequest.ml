open Aws.BaseTypes

type t =
  { reserved_instances_modification_ids : ReservedInstancesModificationIdStringList.t
  ; next_token : String.t option
  ; filters : FilterList.t
  }

let make ?(reserved_instances_modification_ids = []) ?next_token ?(filters = []) () =
  { reserved_instances_modification_ids; next_token; filters }

let parse xml =
  Some
    { reserved_instances_modification_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesModificationId" xml)
             ReservedInstancesModificationIdStringList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesModificationId"
              , ReservedInstancesModificationIdStringList.to_query
                  v.reserved_instances_modification_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ReservedInstancesModificationId"
           , ReservedInstancesModificationIdStringList.to_json
               v.reserved_instances_modification_ids )
       ])

let of_json j =
  { reserved_instances_modification_ids =
      ReservedInstancesModificationIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesModificationId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
