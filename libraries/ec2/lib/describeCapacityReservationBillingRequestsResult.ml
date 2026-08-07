open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; capacity_reservation_billing_requests : CapacityReservationBillingRequestSet.t
  }

let make ?next_token ?(capacity_reservation_billing_requests = []) () =
  { next_token; capacity_reservation_billing_requests }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; capacity_reservation_billing_requests =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationBillingRequestSet" xml)
             CapacityReservationBillingRequestSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationBillingRequestSet"
              , CapacityReservationBillingRequestSet.to_query
                  v.capacity_reservation_billing_requests ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityReservationBillingRequestSet"
           , CapacityReservationBillingRequestSet.to_json
               v.capacity_reservation_billing_requests )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; capacity_reservation_billing_requests =
      CapacityReservationBillingRequestSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "capacityReservationBillingRequestSet"))
  }
