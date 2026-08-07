open Aws.BaseTypes

type t =
  { name : String.t
  ; alternate_name_encoding : String.t option
  ; value : String.t
  ; alternate_value_encoding : String.t option
  }

let make ~name ?alternate_name_encoding ~value ?alternate_value_encoding () =
  { name; alternate_name_encoding; value; alternate_value_encoding }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; alternate_name_encoding =
        Aws.Util.option_bind (Aws.Xml.member "AlternateNameEncoding" xml) String.parse
    ; value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
    ; alternate_value_encoding =
        Aws.Util.option_bind (Aws.Xml.member "AlternateValueEncoding" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alternate_value_encoding (fun f ->
             Aws.Query.Pair ("AlternateValueEncoding", String.to_query f))
       ; Some (Aws.Query.Pair ("Value", String.to_query v.value))
       ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
             Aws.Query.Pair ("AlternateNameEncoding", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alternate_value_encoding (fun f ->
             "AlternateValueEncoding", String.to_json f)
       ; Some ("Value", String.to_json v.value)
       ; Aws.Util.option_map v.alternate_name_encoding (fun f ->
             "AlternateNameEncoding", String.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; alternate_name_encoding =
      Aws.Util.option_map (Aws.Json.lookup j "AlternateNameEncoding") String.of_json
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  ; alternate_value_encoding =
      Aws.Util.option_map (Aws.Json.lookup j "AlternateValueEncoding") String.of_json
  }
