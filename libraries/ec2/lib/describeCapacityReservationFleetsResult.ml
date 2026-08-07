open Aws.BaseTypes

type t =
  { capacity_reservation_fleets : CapacityReservationFleetSet.t
  ; next_token : String.t option
  }

let make ?(capacity_reservation_fleets = []) ?next_token () =
  { capacity_reservation_fleets; next_token }

let parse xml =
  Some
    { capacity_reservation_fleets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationFleetSet" xml)
             CapacityReservationFleetSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationFleetSet"
              , CapacityReservationFleetSet.to_query v.capacity_reservation_fleets ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "capacityReservationFleetSet"
           , CapacityReservationFleetSet.to_json v.capacity_reservation_fleets )
       ])

let of_json j =
  { capacity_reservation_fleets =
      CapacityReservationFleetSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityReservationFleetSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
