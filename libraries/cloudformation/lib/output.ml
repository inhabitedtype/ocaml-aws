open Aws.BaseTypes

type t =
  { output_key : String.t option
  ; output_value : String.t option
  ; description : String.t option
  ; export_name : String.t option
  }

let make ?output_key ?output_value ?description ?export_name () =
  { output_key; output_value; description; export_name }

let parse xml =
  Some
    { output_key = Aws.Util.option_bind (Aws.Xml.member "OutputKey" xml) String.parse
    ; output_value = Aws.Util.option_bind (Aws.Xml.member "OutputValue" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; export_name = Aws.Util.option_bind (Aws.Xml.member "ExportName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.export_name (fun f ->
             Aws.Query.Pair ("ExportName", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.output_value (fun f ->
             Aws.Query.Pair ("OutputValue", String.to_query f))
       ; Aws.Util.option_map v.output_key (fun f ->
             Aws.Query.Pair ("OutputKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.export_name (fun f -> "ExportName", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.output_value (fun f -> "OutputValue", String.to_json f)
       ; Aws.Util.option_map v.output_key (fun f -> "OutputKey", String.to_json f)
       ])

let of_json j =
  { output_key = Aws.Util.option_map (Aws.Json.lookup j "OutputKey") String.of_json
  ; output_value = Aws.Util.option_map (Aws.Json.lookup j "OutputValue") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; export_name = Aws.Util.option_map (Aws.Json.lookup j "ExportName") String.of_json
  }
