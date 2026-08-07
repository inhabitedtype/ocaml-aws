open Aws.BaseTypes

type t =
  { string_value : String.t option
  ; binary_value : Blob.t option
  ; string_list_values : StringList.t
  ; binary_list_values : BinaryList.t
  ; data_type : String.t
  }

let make
    ?string_value
    ?binary_value
    ?(string_list_values = [])
    ?(binary_list_values = [])
    ~data_type
    () =
  { string_value; binary_value; string_list_values; binary_list_values; data_type }

let parse xml =
  Some
    { string_value = Aws.Util.option_bind (Aws.Xml.member "StringValue" xml) String.parse
    ; binary_value = Aws.Util.option_bind (Aws.Xml.member "BinaryValue" xml) Blob.parse
    ; string_list_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StringListValues" xml) StringList.parse)
    ; binary_list_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "BinaryListValues" xml) BinaryList.parse)
    ; data_type =
        Aws.Xml.required
          "DataType"
          (Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DataType", String.to_query v.data_type))
       ; Some
           (Aws.Query.Pair
              ("BinaryListValues.member", BinaryList.to_query v.binary_list_values))
       ; Some
           (Aws.Query.Pair
              ("StringListValues.member", StringList.to_query v.string_list_values))
       ; Aws.Util.option_map v.binary_value (fun f ->
             Aws.Query.Pair ("BinaryValue", Blob.to_query f))
       ; Aws.Util.option_map v.string_value (fun f ->
             Aws.Query.Pair ("StringValue", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DataType", String.to_json v.data_type)
       ; Some ("BinaryListValues", BinaryList.to_json v.binary_list_values)
       ; Some ("StringListValues", StringList.to_json v.string_list_values)
       ; Aws.Util.option_map v.binary_value (fun f -> "BinaryValue", Blob.to_json f)
       ; Aws.Util.option_map v.string_value (fun f -> "StringValue", String.to_json f)
       ])

let of_json j =
  { string_value = Aws.Util.option_map (Aws.Json.lookup j "StringValue") String.of_json
  ; binary_value = Aws.Util.option_map (Aws.Json.lookup j "BinaryValue") Blob.of_json
  ; string_list_values =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StringListValues"))
  ; binary_list_values =
      BinaryList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BinaryListValues"))
  ; data_type = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DataType"))
  }
