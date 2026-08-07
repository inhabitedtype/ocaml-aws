type t =
  { capacity_reservation_cancellation_quote :
      CapacityReservationCancellationQuote.t option
  }

let make ?capacity_reservation_cancellation_quote () =
  { capacity_reservation_cancellation_quote }

let parse xml =
  Some
    { capacity_reservation_cancellation_quote =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationCancellationQuote" xml)
          CapacityReservationCancellationQuote.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_cancellation_quote (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationCancellationQuote"
               , CapacityReservationCancellationQuote.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.capacity_reservation_cancellation_quote (fun f ->
             ( "capacityReservationCancellationQuote"
             , CapacityReservationCancellationQuote.to_json f ))
       ])

let of_json j =
  { capacity_reservation_cancellation_quote =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationCancellationQuote")
        CapacityReservationCancellationQuote.of_json
  }
