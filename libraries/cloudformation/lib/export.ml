open Aws.BaseTypes

type t =
  { exporting_stack_id : String.t option
  ; name : String.t option
  ; value : String.t option
  }

let make ?exporting_stack_id ?name ?value () = { exporting_stack_id; name; value }

let parse xml =
  Some
    { exporting_stack_id =
        Aws.Util.option_bind (Aws.Xml.member "ExportingStackId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.exporting_stack_id (fun f ->
             Aws.Query.Pair ("ExportingStackId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.exporting_stack_id (fun f ->
             "ExportingStackId", String.to_json f)
       ])

let of_json j =
  { exporting_stack_id =
      Aws.Util.option_map (Aws.Json.lookup j "ExportingStackId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  }
