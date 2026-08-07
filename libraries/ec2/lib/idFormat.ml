open Aws.BaseTypes

type t =
  { deadline : DateTime.t option
  ; resource : String.t option
  ; use_long_ids : Boolean.t option
  }

let make ?deadline ?resource ?use_long_ids () = { deadline; resource; use_long_ids }

let parse xml =
  Some
    { deadline = Aws.Util.option_bind (Aws.Xml.member "deadline" xml) DateTime.parse
    ; resource = Aws.Util.option_bind (Aws.Xml.member "resource" xml) String.parse
    ; use_long_ids = Aws.Util.option_bind (Aws.Xml.member "useLongIds" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.use_long_ids (fun f ->
             Aws.Query.Pair ("UseLongIds", Boolean.to_query f))
       ; Aws.Util.option_map v.resource (fun f ->
             Aws.Query.Pair ("Resource", String.to_query f))
       ; Aws.Util.option_map v.deadline (fun f ->
             Aws.Query.Pair ("Deadline", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.use_long_ids (fun f -> "useLongIds", Boolean.to_json f)
       ; Aws.Util.option_map v.resource (fun f -> "resource", String.to_json f)
       ; Aws.Util.option_map v.deadline (fun f -> "deadline", DateTime.to_json f)
       ])

let of_json j =
  { deadline = Aws.Util.option_map (Aws.Json.lookup j "deadline") DateTime.of_json
  ; resource = Aws.Util.option_map (Aws.Json.lookup j "resource") String.of_json
  ; use_long_ids = Aws.Util.option_map (Aws.Json.lookup j "useLongIds") Boolean.of_json
  }
