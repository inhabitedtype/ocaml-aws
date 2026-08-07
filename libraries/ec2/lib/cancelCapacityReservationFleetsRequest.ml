open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; capacity_reservation_fleet_ids : CapacityReservationFleetIdSet.t
  }

let make ?dry_run ~capacity_reservation_fleet_ids () =
  { dry_run; capacity_reservation_fleet_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; capacity_reservation_fleet_ids =
        Aws.Xml.required
          "CapacityReservationFleetId"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationFleetId" xml)
             CapacityReservationFleetIdSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CapacityReservationFleetId"
              , CapacityReservationFleetIdSet.to_query v.capacity_reservation_fleet_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "CapacityReservationFleetId"
           , CapacityReservationFleetIdSet.to_json v.capacity_reservation_fleet_ids )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; capacity_reservation_fleet_ids =
      CapacityReservationFleetIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationFleetId"))
  }
