open Aws.BaseTypes

type t =
  { name : String.t option
  ; value : String.t option
  ; exists : Boolean.t option
  }

let make ?name ?value ?exists () = { name; value; exists }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    ; exists = Aws.Util.option_bind (Aws.Xml.member "Exists" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exists (fun f ->
             Aws.Query.Pair ("Exists", Boolean.to_query f))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exists (fun f -> "Exists", Boolean.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  ; exists = Aws.Util.option_map (Aws.Json.lookup j "Exists") Boolean.of_json
  }
