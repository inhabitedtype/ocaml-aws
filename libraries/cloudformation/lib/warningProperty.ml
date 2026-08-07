open Aws.BaseTypes

type t =
  { property_path : String.t option
  ; required : Boolean.t option
  ; description : String.t option
  }

let make ?property_path ?required ?description () =
  { property_path; required; description }

let parse xml =
  Some
    { property_path =
        Aws.Util.option_bind (Aws.Xml.member "PropertyPath" xml) String.parse
    ; required = Aws.Util.option_bind (Aws.Xml.member "Required" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.required (fun f ->
             Aws.Query.Pair ("Required", Boolean.to_query f))
       ; Aws.Util.option_map v.property_path (fun f ->
             Aws.Query.Pair ("PropertyPath", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.required (fun f -> "Required", Boolean.to_json f)
       ; Aws.Util.option_map v.property_path (fun f -> "PropertyPath", String.to_json f)
       ])

let of_json j =
  { property_path = Aws.Util.option_map (Aws.Json.lookup j "PropertyPath") String.of_json
  ; required = Aws.Util.option_map (Aws.Json.lookup j "Required") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
