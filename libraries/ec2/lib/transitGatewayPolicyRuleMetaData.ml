open Aws.BaseTypes

type t =
  { meta_data_key : String.t option
  ; meta_data_value : String.t option
  }

let make ?meta_data_key ?meta_data_value () = { meta_data_key; meta_data_value }

let parse xml =
  Some
    { meta_data_key = Aws.Util.option_bind (Aws.Xml.member "metaDataKey" xml) String.parse
    ; meta_data_value =
        Aws.Util.option_bind (Aws.Xml.member "metaDataValue" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.meta_data_value (fun f ->
             Aws.Query.Pair ("MetaDataValue", String.to_query f))
       ; Aws.Util.option_map v.meta_data_key (fun f ->
             Aws.Query.Pair ("MetaDataKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.meta_data_value (fun f ->
             "metaDataValue", String.to_json f)
       ; Aws.Util.option_map v.meta_data_key (fun f -> "metaDataKey", String.to_json f)
       ])

let of_json j =
  { meta_data_key = Aws.Util.option_map (Aws.Json.lookup j "metaDataKey") String.of_json
  ; meta_data_value =
      Aws.Util.option_map (Aws.Json.lookup j "metaDataValue") String.of_json
  }
