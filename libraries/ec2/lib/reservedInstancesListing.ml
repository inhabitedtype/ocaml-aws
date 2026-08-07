open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; create_date : DateTime.t option
  ; instance_counts : InstanceCountList.t
  ; price_schedules : PriceScheduleList.t
  ; reserved_instances_id : String.t option
  ; reserved_instances_listing_id : String.t option
  ; status : ListingStatus.t option
  ; status_message : String.t option
  ; tags : TagList.t
  ; update_date : DateTime.t option
  }

let make
    ?client_token
    ?create_date
    ?(instance_counts = [])
    ?(price_schedules = [])
    ?reserved_instances_id
    ?reserved_instances_listing_id
    ?status
    ?status_message
    ?(tags = [])
    ?update_date
    () =
  { client_token
  ; create_date
  ; instance_counts
  ; price_schedules
  ; reserved_instances_id
  ; reserved_instances_listing_id
  ; status
  ; status_message
  ; tags
  ; update_date
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; instance_counts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceCounts" xml)
             InstanceCountList.parse)
    ; price_schedules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "priceSchedules" xml)
             PriceScheduleList.parse)
    ; reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    ; reserved_instances_listing_id =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstancesListingId" xml)
          String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) ListingStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; update_date = Aws.Util.option_bind (Aws.Xml.member "updateDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_date (fun f ->
             Aws.Query.Pair ("UpdateDate", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ListingStatus.to_query f))
       ; Aws.Util.option_map v.reserved_instances_listing_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesListingId", String.to_query f))
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("PriceSchedules", PriceScheduleList.to_query v.price_schedules))
       ; Some
           (Aws.Query.Pair ("InstanceCounts", InstanceCountList.to_query v.instance_counts))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_date (fun f -> "updateDate", DateTime.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", ListingStatus.to_json f)
       ; Aws.Util.option_map v.reserved_instances_listing_id (fun f ->
             "reservedInstancesListingId", String.to_json f)
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ; Some ("priceSchedules", PriceScheduleList.to_json v.price_schedules)
       ; Some ("instanceCounts", InstanceCountList.to_json v.instance_counts)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; instance_counts =
      InstanceCountList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceCounts"))
  ; price_schedules =
      PriceScheduleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "priceSchedules"))
  ; reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  ; reserved_instances_listing_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesListingId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") ListingStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; update_date = Aws.Util.option_map (Aws.Json.lookup j "updateDate") DateTime.of_json
  }
