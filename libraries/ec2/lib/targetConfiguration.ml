open Aws.BaseTypes

type t =
  { instance_count : Integer.t option
  ; offering_id : String.t option
  }

let make ?instance_count ?offering_id () = { instance_count; offering_id }

let parse xml =
  Some
    { instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; offering_id = Aws.Util.option_bind (Aws.Xml.member "offeringId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_id (fun f ->
             Aws.Query.Pair ("OfferingId", String.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_id (fun f -> "offeringId", String.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ])

let of_json j =
  { instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; offering_id = Aws.Util.option_map (Aws.Json.lookup j "offeringId") String.of_json
  }
