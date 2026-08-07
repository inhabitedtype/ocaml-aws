open Aws.BaseTypes

type t =
  { impaired_since : DateTime.t option
  ; name : StatusName.t option
  ; status : StatusType.t option
  }

let make ?impaired_since ?name ?status () = { impaired_since; name; status }

let parse xml =
  Some
    { impaired_since =
        Aws.Util.option_bind (Aws.Xml.member "impairedSince" xml) DateTime.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) StatusName.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) StatusType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StatusType.to_query f))
       ; Aws.Util.option_map v.name (fun f ->
             Aws.Query.Pair ("Name", StatusName.to_query f))
       ; Aws.Util.option_map v.impaired_since (fun f ->
             Aws.Query.Pair ("ImpairedSince", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", StatusType.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", StatusName.to_json f)
       ; Aws.Util.option_map v.impaired_since (fun f ->
             "impairedSince", DateTime.to_json f)
       ])

let of_json j =
  { impaired_since =
      Aws.Util.option_map (Aws.Json.lookup j "impairedSince") DateTime.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") StatusName.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") StatusType.of_json
  }
