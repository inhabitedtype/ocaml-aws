open Aws.BaseTypes

type t =
  { reserved_instances_id : String.t option
  ; reserved_instances_listing_id : String.t option
  ; filters : FilterList.t
  }

let make ?reserved_instances_id ?reserved_instances_listing_id ?(filters = []) () =
  { reserved_instances_id; reserved_instances_listing_id; filters }

let parse xml =
  Some
    { reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    ; reserved_instances_listing_id =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstancesListingId" xml)
          String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.reserved_instances_listing_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesListingId", String.to_query f))
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.reserved_instances_listing_id (fun f ->
             "reservedInstancesListingId", String.to_json f)
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ])

let of_json j =
  { reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  ; reserved_instances_listing_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesListingId") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
