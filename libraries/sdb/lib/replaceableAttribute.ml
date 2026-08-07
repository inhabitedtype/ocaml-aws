open Aws.BaseTypes

type t =
  { name : String.t
  ; value : String.t
  ; replace : Boolean.t option
  }

let make ~name ~value ?replace () = { name; value; replace }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
    ; replace = Aws.Util.option_bind (Aws.Xml.member "Replace" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replace (fun f ->
             Aws.Query.Pair ("Replace", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replace (fun f -> "Replace", Boolean.to_json f)
       ; Some ("Value", String.to_json v.value)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  ; replace = Aws.Util.option_map (Aws.Json.lookup j "Replace") Boolean.of_json
  }
