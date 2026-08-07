open Aws.BaseTypes

type t =
  { instance_count : Integer.t option
  ; offering_id : String.t
  }

let make ?instance_count ~offering_id () = { instance_count; offering_id }

let parse xml =
  Some
    { instance_count =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse
    ; offering_id =
        Aws.Xml.required
          "OfferingId"
          (Aws.Util.option_bind (Aws.Xml.member "OfferingId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("OfferingId", String.to_query v.offering_id))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OfferingId", String.to_json v.offering_id)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "InstanceCount", Integer.to_json f)
       ])

let of_json j =
  { instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCount") Integer.of_json
  ; offering_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OfferingId"))
  }
