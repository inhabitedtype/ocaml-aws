open Aws.BaseTypes

type t =
  { attribute_name : String.t option
  ; attribute_type : String.t option
  ; description : String.t option
  ; default_value : String.t option
  ; cardinality : String.t option
  }

let make ?attribute_name ?attribute_type ?description ?default_value ?cardinality () =
  { attribute_name; attribute_type; description; default_value; cardinality }

let parse xml =
  Some
    { attribute_name =
        Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
    ; attribute_type =
        Aws.Util.option_bind (Aws.Xml.member "AttributeType" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; default_value =
        Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
    ; cardinality = Aws.Util.option_bind (Aws.Xml.member "Cardinality" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cardinality (fun f ->
             Aws.Query.Pair ("Cardinality", String.to_query f))
       ; Aws.Util.option_map v.default_value (fun f ->
             Aws.Query.Pair ("DefaultValue", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.attribute_type (fun f ->
             Aws.Query.Pair ("AttributeType", String.to_query f))
       ; Aws.Util.option_map v.attribute_name (fun f ->
             Aws.Query.Pair ("AttributeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cardinality (fun f -> "Cardinality", String.to_json f)
       ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.attribute_type (fun f -> "AttributeType", String.to_json f)
       ; Aws.Util.option_map v.attribute_name (fun f -> "AttributeName", String.to_json f)
       ])

let of_json j =
  { attribute_name =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
  ; attribute_type =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeType") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; default_value = Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
  ; cardinality = Aws.Util.option_map (Aws.Json.lookup j "Cardinality") String.of_json
  }
