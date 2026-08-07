open Aws.BaseTypes

type t =
  { reservation_value : ReservationValue.t option
  ; reserved_instance_id : String.t option
  }

let make ?reservation_value ?reserved_instance_id () =
  { reservation_value; reserved_instance_id }

let parse xml =
  Some
    { reservation_value =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationValue" xml)
          ReservationValue.parse
    ; reserved_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instance_id (fun f ->
             Aws.Query.Pair ("ReservedInstanceId", String.to_query f))
       ; Aws.Util.option_map v.reservation_value (fun f ->
             Aws.Query.Pair ("ReservationValue", ReservationValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_instance_id (fun f ->
             "reservedInstanceId", String.to_json f)
       ; Aws.Util.option_map v.reservation_value (fun f ->
             "reservationValue", ReservationValue.to_json f)
       ])

let of_json j =
  { reservation_value =
      Aws.Util.option_map (Aws.Json.lookup j "reservationValue") ReservationValue.of_json
  ; reserved_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstanceId") String.of_json
  }
