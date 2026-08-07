open Aws.BaseTypes

type t =
  { count : Integer.t option
  ; version : Integer.t option
  }

let make ?count ?version () = { count; version }

let parse xml =
  Some
    { count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; version = Aws.Util.option_bind (Aws.Xml.member "version" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", Integer.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f -> "version", Integer.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ])

let of_json j =
  { count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; version = Aws.Util.option_map (Aws.Json.lookup j "version") Integer.of_json
  }
