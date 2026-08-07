open Aws.BaseTypes

type t =
  { host_reservation_set : HostReservationSet.t
  ; next_token : String.t option
  }

let make ?(host_reservation_set = []) ?next_token () =
  { host_reservation_set; next_token }

let parse xml =
  Some
    { host_reservation_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "hostReservationSet" xml)
             HostReservationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("HostReservationSet", HostReservationSet.to_query v.host_reservation_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("hostReservationSet", HostReservationSet.to_json v.host_reservation_set)
       ])

let of_json j =
  { host_reservation_set =
      HostReservationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "hostReservationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
