open Aws.BaseTypes

type t =
  { capacity_reservation_cancellation_quotes :
      CapacityReservationCancellationQuoteResponseSet.t
  ; next_token : String.t option
  }

let make ?(capacity_reservation_cancellation_quotes = []) ?next_token () =
  { capacity_reservation_cancellation_quotes; next_token }

let parse xml =
  Some
    { capacity_reservation_cancellation_quotes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationCancellationQuoteSet" xml)
             CapacityReservationCancellationQuoteResponseSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationCancellationQuoteSet"
              , CapacityReservationCancellationQuoteResponseSet.to_query
                  v.capacity_reservation_cancellation_quotes ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "capacityReservationCancellationQuoteSet"
           , CapacityReservationCancellationQuoteResponseSet.to_json
               v.capacity_reservation_cancellation_quotes )
       ])

let of_json j =
  { capacity_reservation_cancellation_quotes =
      CapacityReservationCancellationQuoteResponseSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "capacityReservationCancellationQuoteSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
