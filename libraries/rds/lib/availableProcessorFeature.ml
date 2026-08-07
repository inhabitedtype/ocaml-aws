open Aws.BaseTypes

type t =
  { name : String.t option
  ; default_value : String.t option
  ; allowed_values : String.t option
  }

let make ?name ?default_value ?allowed_values () = { name; default_value; allowed_values }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; default_value =
        Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
    ; allowed_values =
        Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_values (fun f ->
             Aws.Query.Pair ("AllowedValues", String.to_query f))
       ; Aws.Util.option_map v.default_value (fun f ->
             Aws.Query.Pair ("DefaultValue", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.allowed_values (fun f -> "AllowedValues", String.to_json f)
       ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; default_value = Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
  ; allowed_values =
      Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
  }
