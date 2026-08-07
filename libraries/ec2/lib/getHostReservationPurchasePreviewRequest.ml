open Aws.BaseTypes

type t =
  { host_id_set : RequestHostIdSet.t
  ; offering_id : String.t
  }

let make ~host_id_set ~offering_id () = { host_id_set; offering_id }

let parse xml =
  Some
    { host_id_set =
        Aws.Xml.required
          "HostIdSet"
          (Aws.Util.option_bind (Aws.Xml.member "HostIdSet" xml) RequestHostIdSet.parse)
    ; offering_id =
        Aws.Xml.required
          "OfferingId"
          (Aws.Util.option_bind (Aws.Xml.member "OfferingId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("OfferingId", String.to_query v.offering_id))
       ; Some (Aws.Query.Pair ("HostIdSet", RequestHostIdSet.to_query v.host_id_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OfferingId", String.to_json v.offering_id)
       ; Some ("HostIdSet", RequestHostIdSet.to_json v.host_id_set)
       ])

let of_json j =
  { host_id_set =
      RequestHostIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostIdSet"))
  ; offering_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OfferingId"))
  }
