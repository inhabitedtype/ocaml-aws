open Aws.BaseTypes

type t =
  { source_capacity_reservation_id : String.t option
  ; interruption_type : InterruptionType.t option
  }

let make ?source_capacity_reservation_id ?interruption_type () =
  { source_capacity_reservation_id; interruption_type }

let parse xml =
  Some
    { source_capacity_reservation_id =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceCapacityReservationId" xml)
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
       ; Aws.Util.option_map v.source_capacity_reservation_id (fun f ->
             Aws.Query.Pair ("SourceCapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_type (fun f ->
             "interruptionType", InterruptionType.to_json f)
       ; Aws.Util.option_map v.source_capacity_reservation_id (fun f ->
             "sourceCapacityReservationId", String.to_json f)
       ])

let of_json j =
  { source_capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceCapacityReservationId") String.of_json
  ; interruption_type =
      Aws.Util.option_map (Aws.Json.lookup j "interruptionType") InterruptionType.of_json
  }
