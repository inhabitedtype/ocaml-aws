open Aws.BaseTypes

type t =
  { instance_count : Integer.t option
  ; target_instance_count : Integer.t option
  ; status : InterruptibleCapacityReservationAllocationStatus.t option
  ; interruptible_capacity_reservation_id : String.t option
  ; interruption_type : InterruptionType.t option
  }

let make
    ?instance_count
    ?target_instance_count
    ?status
    ?interruptible_capacity_reservation_id
    ?interruption_type
    () =
  { instance_count
  ; target_instance_count
  ; status
  ; interruptible_capacity_reservation_id
  ; interruption_type
  }

let parse xml =
  Some
    { instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; target_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "targetInstanceCount" xml) Integer.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          InterruptibleCapacityReservationAllocationStatus.parse
    ; interruptible_capacity_reservation_id =
        Aws.Util.option_bind
          (Aws.Xml.member "interruptibleCapacityReservationId" xml)
          String.parse
    ; interruption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "interruptionType" xml)
          InterruptionType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_type (fun f ->
             Aws.Query.Pair ("InterruptionType", InterruptionType.to_query f))
       ; Aws.Util.option_map v.interruptible_capacity_reservation_id (fun f ->
             Aws.Query.Pair ("InterruptibleCapacityReservationId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair
               ("Status", InterruptibleCapacityReservationAllocationStatus.to_query f))
       ; Aws.Util.option_map v.target_instance_count (fun f ->
             Aws.Query.Pair ("TargetInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_type (fun f ->
             "interruptionType", InterruptionType.to_json f)
       ; Aws.Util.option_map v.interruptible_capacity_reservation_id (fun f ->
             "interruptibleCapacityReservationId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", InterruptibleCapacityReservationAllocationStatus.to_json f)
       ; Aws.Util.option_map v.target_instance_count (fun f ->
             "targetInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ])

let of_json j =
  { instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; target_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "targetInstanceCount") Integer.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        InterruptibleCapacityReservationAllocationStatus.of_json
  ; interruptible_capacity_reservation_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "interruptibleCapacityReservationId")
        String.of_json
  ; interruption_type =
      Aws.Util.option_map (Aws.Json.lookup j "interruptionType") InterruptionType.of_json
  }
