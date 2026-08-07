open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; capacity_reservations : CapacityReservationSet.t
  }

let make ?next_token ?(capacity_reservations = []) () =
  { next_token; capacity_reservations }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; capacity_reservations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityReservationSet" xml)
             CapacityReservationSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationSet"
              , CapacityReservationSet.to_query v.capacity_reservations ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "capacityReservationSet"
           , CapacityReservationSet.to_json v.capacity_reservations )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; capacity_reservations =
      CapacityReservationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityReservationSet"))
  }
