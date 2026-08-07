open Aws.BaseTypes

type t =
  { instance_count : Integer.t option
  ; state : ListingState.t option
  }

let make ?instance_count ?state () = { instance_count; state }

let parse xml =
  Some
    { instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) ListingState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", ListingState.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", ListingState.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ])

let of_json j =
  { instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") ListingState.of_json
  }
