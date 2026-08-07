open Aws.BaseTypes

type t =
  { capacity_block_extension_offering_id : String.t
  ; capacity_reservation_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~capacity_block_extension_offering_id ~capacity_reservation_id ?dry_run () =
  { capacity_block_extension_offering_id; capacity_reservation_id; dry_run }

let parse xml =
  Some
    { capacity_block_extension_offering_id =
        Aws.Xml.required
          "CapacityBlockExtensionOfferingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityBlockExtensionOfferingId" xml)
             String.parse)
    ; capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ; Some
           (Aws.Query.Pair
              ( "CapacityBlockExtensionOfferingId"
              , String.to_query v.capacity_block_extension_offering_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ; Some
           ( "CapacityBlockExtensionOfferingId"
           , String.to_json v.capacity_block_extension_offering_id )
       ])

let of_json j =
  { capacity_block_extension_offering_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityBlockExtensionOfferingId"))
  ; capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
