open Aws.BaseTypes

type t =
  { source_capacity_reservation : CapacityReservation.t option
  ; destination_capacity_reservation : CapacityReservation.t option
  ; instance_count : Integer.t option
  }

let make ?source_capacity_reservation ?destination_capacity_reservation ?instance_count ()
    =
  { source_capacity_reservation; destination_capacity_reservation; instance_count }

let parse xml =
  Some
    { source_capacity_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceCapacityReservation" xml)
          CapacityReservation.parse
    ; destination_capacity_reservation =
        Aws.Util.option_bind
          (Aws.Xml.member "destinationCapacityReservation" xml)
          CapacityReservation.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.destination_capacity_reservation (fun f ->
             Aws.Query.Pair
               ("DestinationCapacityReservation", CapacityReservation.to_query f))
       ; Aws.Util.option_map v.source_capacity_reservation (fun f ->
             Aws.Query.Pair ("SourceCapacityReservation", CapacityReservation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.destination_capacity_reservation (fun f ->
             "destinationCapacityReservation", CapacityReservation.to_json f)
       ; Aws.Util.option_map v.source_capacity_reservation (fun f ->
             "sourceCapacityReservation", CapacityReservation.to_json f)
       ])

let of_json j =
  { source_capacity_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceCapacityReservation")
        CapacityReservation.of_json
  ; destination_capacity_reservation =
      Aws.Util.option_map
        (Aws.Json.lookup j "destinationCapacityReservation")
        CapacityReservation.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  }
