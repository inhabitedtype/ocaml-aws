open Aws.BaseTypes

type t =
  { filter : FilterList.t
  ; host_reservation_id_set : HostReservationIdSet.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?(filter = []) ?(host_reservation_id_set = []) ?max_results ?next_token () =
  { filter; host_reservation_id_set; max_results; next_token }

let parse xml =
  Some
    { filter =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; host_reservation_id_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "HostReservationIdSet" xml)
             HostReservationIdSet.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "HostReservationIdSet"
              , HostReservationIdSet.to_query v.host_reservation_id_set ))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filter))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ("HostReservationIdSet", HostReservationIdSet.to_json v.host_reservation_id_set)
       ; Some ("Filter", FilterList.to_json v.filter)
       ])

let of_json j =
  { filter = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; host_reservation_id_set =
      HostReservationIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "HostReservationIdSet"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
