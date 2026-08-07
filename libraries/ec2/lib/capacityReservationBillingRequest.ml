open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; requested_by : String.t option
  ; unused_reservation_billing_owner_id : String.t option
  ; last_update_time : DateTime.t option
  ; status : CapacityReservationBillingRequestStatus.t option
  ; status_message : String.t option
  ; capacity_reservation_info : CapacityReservationInfo.t option
  }

let make
    ?capacity_reservation_id
    ?requested_by
    ?unused_reservation_billing_owner_id
    ?last_update_time
    ?status
    ?status_message
    ?capacity_reservation_info
    () =
  { capacity_reservation_id
  ; requested_by
  ; unused_reservation_billing_owner_id
  ; last_update_time
  ; status
  ; status_message
  ; capacity_reservation_info
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; requested_by = Aws.Util.option_bind (Aws.Xml.member "requestedBy" xml) String.parse
    ; unused_reservation_billing_owner_id =
        Aws.Util.option_bind
          (Aws.Xml.member "unusedReservationBillingOwnerId" xml)
          String.parse
    ; last_update_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdateTime" xml) DateTime.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          CapacityReservationBillingRequestStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; capacity_reservation_info =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationInfo" xml)
          CapacityReservationInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_info (fun f ->
             Aws.Query.Pair ("CapacityReservationInfo", CapacityReservationInfo.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", CapacityReservationBillingRequestStatus.to_query f))
       ; Aws.Util.option_map v.last_update_time (fun f ->
             Aws.Query.Pair ("LastUpdateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.unused_reservation_billing_owner_id (fun f ->
             Aws.Query.Pair ("UnusedReservationBillingOwnerId", String.to_query f))
       ; Aws.Util.option_map v.requested_by (fun f ->
             Aws.Query.Pair ("RequestedBy", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_info (fun f ->
             "capacityReservationInfo", CapacityReservationInfo.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", CapacityReservationBillingRequestStatus.to_json f)
       ; Aws.Util.option_map v.last_update_time (fun f ->
             "lastUpdateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.unused_reservation_billing_owner_id (fun f ->
             "unusedReservationBillingOwnerId", String.to_json f)
       ; Aws.Util.option_map v.requested_by (fun f -> "requestedBy", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; requested_by = Aws.Util.option_map (Aws.Json.lookup j "requestedBy") String.of_json
  ; unused_reservation_billing_owner_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "unusedReservationBillingOwnerId")
        String.of_json
  ; last_update_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdateTime") DateTime.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        CapacityReservationBillingRequestStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; capacity_reservation_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationInfo")
        CapacityReservationInfo.of_json
  }
