type t =
  { reservation_value : ReservationValue.t option
  ; target_configuration : TargetConfiguration.t option
  }

let make ?reservation_value ?target_configuration () =
  { reservation_value; target_configuration }

let parse xml =
  Some
    { reservation_value =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationValue" xml)
          ReservationValue.parse
    ; target_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "targetConfiguration" xml)
          TargetConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_configuration (fun f ->
             Aws.Query.Pair ("TargetConfiguration", TargetConfiguration.to_query f))
       ; Aws.Util.option_map v.reservation_value (fun f ->
             Aws.Query.Pair ("ReservationValue", ReservationValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_configuration (fun f ->
             "targetConfiguration", TargetConfiguration.to_json f)
       ; Aws.Util.option_map v.reservation_value (fun f ->
             "reservationValue", ReservationValue.to_json f)
       ])

let of_json j =
  { reservation_value =
      Aws.Util.option_map (Aws.Json.lookup j "reservationValue") ReservationValue.of_json
  ; target_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetConfiguration")
        TargetConfiguration.of_json
  }
