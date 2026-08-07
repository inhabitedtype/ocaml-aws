open Aws.BaseTypes

type t =
  { value : String.t option
  ; status : String.t option
  ; reason : String.t option
  }

let make ?value ?status ?reason () = { value; status; reason }

let parse xml =
  Some
    { value = Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "reason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reason (fun f -> "reason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "value", String.to_json f)
       ])

let of_json j =
  { value = Aws.Util.option_map (Aws.Json.lookup j "value") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "reason") String.of_json
  }
