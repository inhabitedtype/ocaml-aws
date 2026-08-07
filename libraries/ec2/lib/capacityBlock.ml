open Aws.BaseTypes

type t =
  { capacity_block_id : String.t option
  ; ultraserver_type : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; capacity_reservation_ids : CapacityReservationIdSet.t
  ; start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; create_date : DateTime.t option
  ; state : CapacityBlockResourceState.t option
  ; tags : TagList.t
  }

let make
    ?capacity_block_id
    ?ultraserver_type
    ?availability_zone
    ?availability_zone_id
    ?(capacity_reservation_ids = [])
    ?start_date
    ?end_date
    ?create_date
    ?state
    ?(tags = [])
    () =
  { capacity_block_id
  ; ultraserver_type
  ; availability_zone
  ; availability_zone_id
  ; capacity_reservation_ids
  ; start_date
  ; end_date
  ; create_date
  ; state
  ; tags
  }

let parse xml =
  Some
    { capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    ; ultraserver_type =
        Aws.Util.option_bind (Aws.Xml.member "ultraserverType" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; capacity_reservation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationIdSet" xml)
             CapacityReservationIdSet.parse)
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) CapacityBlockResourceState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CapacityBlockResourceState.to_query f))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationIdSet"
              , CapacityReservationIdSet.to_query v.capacity_reservation_ids ))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             Aws.Query.Pair ("UltraserverType", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", CapacityBlockResourceState.to_json f)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Some
           ( "capacityReservationIdSet"
           , CapacityReservationIdSet.to_json v.capacity_reservation_ids )
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             "ultraserverType", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ])

let of_json j =
  { capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  ; ultraserver_type =
      Aws.Util.option_map (Aws.Json.lookup j "ultraserverType") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; capacity_reservation_ids =
      CapacityReservationIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityReservationIdSet"))
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") CapacityBlockResourceState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
