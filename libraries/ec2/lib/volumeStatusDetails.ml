open Aws.BaseTypes

type t =
  { name : VolumeStatusName.t option
  ; status : String.t option
  }

let make ?name ?status () = { name; status }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "name" xml) VolumeStatusName.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.name (fun f ->
             Aws.Query.Pair ("Name", VolumeStatusName.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", VolumeStatusName.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "name") VolumeStatusName.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  }
