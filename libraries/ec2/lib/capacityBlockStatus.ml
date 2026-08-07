open Aws.BaseTypes

type t =
  { capacity_block_id : String.t option
  ; interconnect_status : CapacityBlockInterconnectStatus.t option
  ; total_capacity : Integer.t option
  ; total_available_capacity : Integer.t option
  ; total_unavailable_capacity : Integer.t option
  ; capacity_reservation_statuses : CapacityReservationStatusSet.t
  }

let make
    ?capacity_block_id
    ?interconnect_status
    ?total_capacity
    ?total_available_capacity
    ?total_unavailable_capacity
    ?(capacity_reservation_statuses = [])
    () =
  { capacity_block_id
  ; interconnect_status
  ; total_capacity
  ; total_available_capacity
  ; total_unavailable_capacity
  ; capacity_reservation_statuses
  }

let parse xml =
  Some
    { capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    ; interconnect_status =
        Aws.Util.option_bind
          (Aws.Xml.member "interconnectStatus" xml)
          CapacityBlockInterconnectStatus.parse
    ; total_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalCapacity" xml) Integer.parse
    ; total_available_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalAvailableCapacity" xml) Integer.parse
    ; total_unavailable_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalUnavailableCapacity" xml) Integer.parse
    ; capacity_reservation_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationStatusSet" xml)
             CapacityReservationStatusSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationStatusSet"
              , CapacityReservationStatusSet.to_query v.capacity_reservation_statuses ))
       ; Aws.Util.option_map v.total_unavailable_capacity (fun f ->
             Aws.Query.Pair ("TotalUnavailableCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.total_available_capacity (fun f ->
             Aws.Query.Pair ("TotalAvailableCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.total_capacity (fun f ->
             Aws.Query.Pair ("TotalCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.interconnect_status (fun f ->
             Aws.Query.Pair
               ("InterconnectStatus", CapacityBlockInterconnectStatus.to_query f))
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityReservationStatusSet"
           , CapacityReservationStatusSet.to_json v.capacity_reservation_statuses )
       ; Aws.Util.option_map v.total_unavailable_capacity (fun f ->
             "totalUnavailableCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.total_available_capacity (fun f ->
             "totalAvailableCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.total_capacity (fun f ->
             "totalCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.interconnect_status (fun f ->
             "interconnectStatus", CapacityBlockInterconnectStatus.to_json f)
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ])

let of_json j =
  { capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  ; interconnect_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "interconnectStatus")
        CapacityBlockInterconnectStatus.of_json
  ; total_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalCapacity") Integer.of_json
  ; total_available_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalAvailableCapacity") Integer.of_json
  ; total_unavailable_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalUnavailableCapacity") Integer.of_json
  ; capacity_reservation_statuses =
      CapacityReservationStatusSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityReservationStatusSet"))
  }
