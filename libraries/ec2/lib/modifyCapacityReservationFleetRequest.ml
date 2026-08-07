open Aws.BaseTypes

type t =
  { capacity_reservation_fleet_id : String.t
  ; total_target_capacity : Integer.t option
  ; end_date : DateTime.t option
  ; dry_run : Boolean.t option
  ; remove_end_date : Boolean.t option
  }

let make
    ~capacity_reservation_fleet_id
    ?total_target_capacity
    ?end_date
    ?dry_run
    ?remove_end_date
    () =
  { capacity_reservation_fleet_id
  ; total_target_capacity
  ; end_date
  ; dry_run
  ; remove_end_date
  }

let parse xml =
  Some
    { capacity_reservation_fleet_id =
        Aws.Xml.required
          "CapacityReservationFleetId"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationFleetId" xml)
             String.parse)
    ; total_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "TotalTargetCapacity" xml) Integer.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; remove_end_date =
        Aws.Util.option_bind (Aws.Xml.member "RemoveEndDate" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_end_date (fun f ->
             Aws.Query.Pair ("RemoveEndDate", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             Aws.Query.Pair ("TotalTargetCapacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationFleetId"
              , String.to_query v.capacity_reservation_fleet_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.remove_end_date (fun f ->
             "RemoveEndDate", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             "TotalTargetCapacity", Integer.to_json f)
       ; Some
           ("CapacityReservationFleetId", String.to_json v.capacity_reservation_fleet_id)
       ])

let of_json j =
  { capacity_reservation_fleet_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationFleetId"))
  ; total_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "TotalTargetCapacity") Integer.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; remove_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "RemoveEndDate") Boolean.of_json
  }
