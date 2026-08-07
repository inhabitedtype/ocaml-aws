open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; capacity_reservation_groups : CapacityReservationGroupSet.t
  }

let make ?next_token ?(capacity_reservation_groups = []) () =
  { next_token; capacity_reservation_groups }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; capacity_reservation_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationGroupSet" xml)
             CapacityReservationGroupSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationGroupSet"
              , CapacityReservationGroupSet.to_query v.capacity_reservation_groups ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityReservationGroupSet"
           , CapacityReservationGroupSet.to_json v.capacity_reservation_groups )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; capacity_reservation_groups =
      CapacityReservationGroupSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityReservationGroupSet"))
  }
