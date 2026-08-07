open Aws.BaseTypes

type t =
  { rule_name : String.t
  ; rule_state : String.t option
  ; rule_definition : String.t
  ; tags : TagList.t
  ; apply_on_transformed_logs : Boolean.t option
  }

let make
    ~rule_name
    ?rule_state
    ~rule_definition
    ?(tags = [])
    ?apply_on_transformed_logs
    () =
  { rule_name; rule_state; rule_definition; tags; apply_on_transformed_logs }

let parse xml =
  Some
    { rule_name =
        Aws.Xml.required
          "RuleName"
          (Aws.Util.option_bind (Aws.Xml.member "RuleName" xml) String.parse)
    ; rule_state = Aws.Util.option_bind (Aws.Xml.member "RuleState" xml) String.parse
    ; rule_definition =
        Aws.Xml.required
          "RuleDefinition"
          (Aws.Util.option_bind (Aws.Xml.member "RuleDefinition" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; apply_on_transformed_logs =
        Aws.Util.option_bind (Aws.Xml.member "ApplyOnTransformedLogs" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_on_transformed_logs (fun f ->
             Aws.Query.Pair ("ApplyOnTransformedLogs", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("RuleDefinition", String.to_query v.rule_definition))
       ; Aws.Util.option_map v.rule_state (fun f ->
             Aws.Query.Pair ("RuleState", String.to_query f))
       ; Some (Aws.Query.Pair ("RuleName", String.to_query v.rule_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_on_transformed_logs (fun f ->
             "ApplyOnTransformedLogs", Boolean.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some ("RuleDefinition", String.to_json v.rule_definition)
       ; Aws.Util.option_map v.rule_state (fun f -> "RuleState", String.to_json f)
       ; Some ("RuleName", String.to_json v.rule_name)
       ])

let of_json j =
  { rule_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleName"))
  ; rule_state = Aws.Util.option_map (Aws.Json.lookup j "RuleState") String.of_json
  ; rule_definition =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleDefinition"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; apply_on_transformed_logs =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyOnTransformedLogs") Boolean.of_json
  }
