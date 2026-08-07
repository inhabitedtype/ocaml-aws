open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; capacity_reservation_id : String.t
  ; unused_reservation_billing_owner_id : String.t
  }

let make ?dry_run ~capacity_reservation_id ~unused_reservation_billing_owner_id () =
  { dry_run; capacity_reservation_id; unused_reservation_billing_owner_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; unused_reservation_billing_owner_id =
        Aws.Xml.required
          "UnusedReservationBillingOwnerId"
          (Aws.Util.option_bind
             (Aws.Xml.member "UnusedReservationBillingOwnerId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnusedReservationBillingOwnerId"
              , String.to_query v.unused_reservation_billing_owner_id ))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "UnusedReservationBillingOwnerId"
           , String.to_json v.unused_reservation_billing_owner_id )
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; unused_reservation_billing_owner_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UnusedReservationBillingOwnerId"))
  }
