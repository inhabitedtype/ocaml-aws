open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t
  ; target_instance_count : Integer.t
  ; dry_run : Boolean.t option
  }

let make ~capacity_reservation_id ~target_instance_count ?dry_run () =
  { capacity_reservation_id; target_instance_count; dry_run }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; target_instance_count =
        Aws.Xml.required
          "TargetInstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "TargetInstanceCount" xml) Integer.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TargetInstanceCount", Integer.to_query v.target_instance_count))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TargetInstanceCount", Integer.to_json v.target_instance_count)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ])

let of_json j =
  { capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; target_instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetInstanceCount"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
