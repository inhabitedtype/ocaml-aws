open Aws.BaseTypes

type t =
  { name : String.t option
  ; value : String.t option
  ; default_value : String.t option
  ; description : String.t option
  ; apply_type : String.t option
  ; data_type : String.t option
  ; allowed_values : String.t option
  ; is_modifiable : Boolean.t option
  ; is_collection : Boolean.t option
  }

let make
    ?name
    ?value
    ?default_value
    ?description
    ?apply_type
    ?data_type
    ?allowed_values
    ?is_modifiable
    ?is_collection
    () =
  { name
  ; value
  ; default_value
  ; description
  ; apply_type
  ; data_type
  ; allowed_values
  ; is_modifiable
  ; is_collection
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    ; default_value =
        Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; apply_type = Aws.Util.option_bind (Aws.Xml.member "ApplyType" xml) String.parse
    ; data_type = Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse
    ; allowed_values =
        Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
    ; is_modifiable =
        Aws.Util.option_bind (Aws.Xml.member "IsModifiable" xml) Boolean.parse
    ; is_collection =
        Aws.Util.option_bind (Aws.Xml.member "IsCollection" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_collection (fun f ->
             Aws.Query.Pair ("IsCollection", Boolean.to_query f))
       ; Aws.Util.option_map v.is_modifiable (fun f ->
             Aws.Query.Pair ("IsModifiable", Boolean.to_query f))
       ; Aws.Util.option_map v.allowed_values (fun f ->
             Aws.Query.Pair ("AllowedValues", String.to_query f))
       ; Aws.Util.option_map v.data_type (fun f ->
             Aws.Query.Pair ("DataType", String.to_query f))
       ; Aws.Util.option_map v.apply_type (fun f ->
             Aws.Query.Pair ("ApplyType", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.default_value (fun f ->
             Aws.Query.Pair ("DefaultValue", String.to_query f))
       ; Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_collection (fun f -> "IsCollection", Boolean.to_json f)
       ; Aws.Util.option_map v.is_modifiable (fun f -> "IsModifiable", Boolean.to_json f)
       ; Aws.Util.option_map v.allowed_values (fun f -> "AllowedValues", String.to_json f)
       ; Aws.Util.option_map v.data_type (fun f -> "DataType", String.to_json f)
       ; Aws.Util.option_map v.apply_type (fun f -> "ApplyType", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
       ; Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  ; default_value = Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; apply_type = Aws.Util.option_map (Aws.Json.lookup j "ApplyType") String.of_json
  ; data_type = Aws.Util.option_map (Aws.Json.lookup j "DataType") String.of_json
  ; allowed_values =
      Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
  ; is_modifiable = Aws.Util.option_map (Aws.Json.lookup j "IsModifiable") Boolean.of_json
  ; is_collection = Aws.Util.option_map (Aws.Json.lookup j "IsCollection") Boolean.of_json
  }
