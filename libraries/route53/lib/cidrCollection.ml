open Aws.BaseTypes

type t =
  { arn : String.t option
  ; id : String.t option
  ; name : String.t option
  ; version : Long.t option
  }

let make ?arn ?id ?name ?version () = { arn; id; name; version }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; version = Aws.Util.option_bind (Aws.Xml.member "Version" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", Long.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f -> "Version", Long.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; version = Aws.Util.option_map (Aws.Json.lookup j "Version") Long.of_json
  }
