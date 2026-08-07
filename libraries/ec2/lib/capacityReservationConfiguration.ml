open Aws.BaseTypes

type t =
  { instance_count : Integer.t option
  ; reservation_state : String.t option
  }

let make ?instance_count ?reservation_state () = { instance_count; reservation_state }

let parse xml =
  Some
    { instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; reservation_state =
        Aws.Util.option_bind (Aws.Xml.member "reservationState" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reservation_state (fun f ->
             Aws.Query.Pair ("ReservationState", String.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reservation_state (fun f ->
             "reservationState", String.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ])

let of_json j =
  { instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; reservation_state =
      Aws.Util.option_map (Aws.Json.lookup j "reservationState") String.of_json
  }
