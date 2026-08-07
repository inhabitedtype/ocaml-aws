open Aws.BaseTypes

type t =
  { name : String.t
  ; state : String.t
  ; schema : String.t
  ; definition : String.t
  ; managed_rule : Boolean.t option
  ; apply_on_transformed_logs : Boolean.t option
  }

let make ~name ~state ~schema ~definition ?managed_rule ?apply_on_transformed_logs () =
  { name; state; schema; definition; managed_rule; apply_on_transformed_logs }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; state =
        Aws.Xml.required
          "State"
          (Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse)
    ; schema =
        Aws.Xml.required
          "Schema"
          (Aws.Util.option_bind (Aws.Xml.member "Schema" xml) String.parse)
    ; definition =
        Aws.Xml.required
          "Definition"
          (Aws.Util.option_bind (Aws.Xml.member "Definition" xml) String.parse)
    ; managed_rule = Aws.Util.option_bind (Aws.Xml.member "ManagedRule" xml) Boolean.parse
    ; apply_on_transformed_logs =
        Aws.Util.option_bind (Aws.Xml.member "ApplyOnTransformedLogs" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_on_transformed_logs (fun f ->
             Aws.Query.Pair ("ApplyOnTransformedLogs", Boolean.to_query f))
       ; Aws.Util.option_map v.managed_rule (fun f ->
             Aws.Query.Pair ("ManagedRule", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Definition", String.to_query v.definition))
       ; Some (Aws.Query.Pair ("Schema", String.to_query v.schema))
       ; Some (Aws.Query.Pair ("State", String.to_query v.state))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_on_transformed_logs (fun f ->
             "ApplyOnTransformedLogs", Boolean.to_json f)
       ; Aws.Util.option_map v.managed_rule (fun f -> "ManagedRule", Boolean.to_json f)
       ; Some ("Definition", String.to_json v.definition)
       ; Some ("Schema", String.to_json v.schema)
       ; Some ("State", String.to_json v.state)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; state = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "State"))
  ; schema = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Schema"))
  ; definition = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Definition"))
  ; managed_rule = Aws.Util.option_map (Aws.Json.lookup j "ManagedRule") Boolean.of_json
  ; apply_on_transformed_logs =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyOnTransformedLogs") Boolean.of_json
  }
