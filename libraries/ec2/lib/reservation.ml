open Aws.BaseTypes

type t =
  { reservation_id : String.t
  ; owner_id : String.t
  ; requester_id : String.t option
  ; groups : GroupIdentifierList.t
  ; instances : InstanceList.t
  }

let make ~reservation_id ~owner_id ?requester_id ?(groups = []) ?(instances = []) () =
  { reservation_id; owner_id; requester_id; groups; instances }

let parse xml =
  Some
    { reservation_id =
        Aws.Xml.required
          "reservationId"
          (Aws.Util.option_bind (Aws.Xml.member "reservationId" xml) String.parse)
    ; owner_id =
        Aws.Xml.required
          "ownerId"
          (Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse)
    ; requester_id = Aws.Util.option_bind (Aws.Xml.member "requesterId" xml) String.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "instancesSet" xml) InstanceList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("InstancesSet", InstanceList.to_query v.instances))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ; Aws.Util.option_map v.requester_id (fun f ->
             Aws.Query.Pair ("RequesterId", String.to_query f))
       ; Some (Aws.Query.Pair ("OwnerId", String.to_query v.owner_id))
       ; Some (Aws.Query.Pair ("ReservationId", String.to_query v.reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("instancesSet", InstanceList.to_json v.instances)
       ; Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ; Aws.Util.option_map v.requester_id (fun f -> "requesterId", String.to_json f)
       ; Some ("ownerId", String.to_json v.owner_id)
       ; Some ("reservationId", String.to_json v.reservation_id)
       ])

let of_json j =
  { reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "reservationId"))
  ; owner_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ownerId"))
  ; requester_id = Aws.Util.option_map (Aws.Json.lookup j "requesterId") String.of_json
  ; groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; instances =
      InstanceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instancesSet"))
  }
