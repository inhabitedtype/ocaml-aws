open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; capacity_reservation_resource_group_arn : String.t option
  }

let make ?capacity_reservation_id ?capacity_reservation_resource_group_arn () =
  { capacity_reservation_id; capacity_reservation_resource_group_arn }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse
    ; capacity_reservation_resource_group_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationResourceGroupArn" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_resource_group_arn (fun f ->
             Aws.Query.Pair ("CapacityReservationResourceGroupArn", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_resource_group_arn (fun f ->
             "CapacityReservationResourceGroupArn", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "CapacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "CapacityReservationId") String.of_json
  ; capacity_reservation_resource_group_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationResourceGroupArn")
        String.of_json
  }
