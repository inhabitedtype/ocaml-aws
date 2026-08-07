open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; capacity_reservation_specification : CapacityReservationSpecification.t
  ; dry_run : Boolean.t option
  }

let make ~instance_id ~capacity_reservation_specification ?dry_run () =
  { instance_id; capacity_reservation_specification; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; capacity_reservation_specification =
        Aws.Xml.required
          "CapacityReservationSpecification"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationSpecification" xml)
             CapacityReservationSpecification.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationSpecification"
              , CapacityReservationSpecification.to_query
                  v.capacity_reservation_specification ))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "CapacityReservationSpecification"
           , CapacityReservationSpecification.to_json v.capacity_reservation_specification
           )
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; capacity_reservation_specification =
      CapacityReservationSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
