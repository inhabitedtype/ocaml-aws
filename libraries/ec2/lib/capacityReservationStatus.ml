open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; total_capacity : Integer.t option
  ; total_available_capacity : Integer.t option
  ; total_unavailable_capacity : Integer.t option
  }

let make
    ?capacity_reservation_id
    ?total_capacity
    ?total_available_capacity
    ?total_unavailable_capacity
    () =
  { capacity_reservation_id
  ; total_capacity
  ; total_available_capacity
  ; total_unavailable_capacity
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; total_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalCapacity" xml) Integer.parse
    ; total_available_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalAvailableCapacity" xml) Integer.parse
    ; total_unavailable_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalUnavailableCapacity" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_unavailable_capacity (fun f ->
             Aws.Query.Pair ("TotalUnavailableCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.total_available_capacity (fun f ->
             Aws.Query.Pair ("TotalAvailableCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.total_capacity (fun f ->
             Aws.Query.Pair ("TotalCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_unavailable_capacity (fun f ->
             "totalUnavailableCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.total_available_capacity (fun f ->
             "totalAvailableCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.total_capacity (fun f ->
             "totalCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; total_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalCapacity") Integer.of_json
  ; total_available_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalAvailableCapacity") Integer.of_json
  ; total_unavailable_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalUnavailableCapacity") Integer.of_json
  }
