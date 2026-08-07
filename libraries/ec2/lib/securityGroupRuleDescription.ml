open Aws.BaseTypes

type t =
  { security_group_rule_id : String.t option
  ; description : String.t option
  }

let make ?security_group_rule_id ?description () = { security_group_rule_id; description }

let parse xml =
  Some
    { security_group_rule_id =
        Aws.Util.option_bind (Aws.Xml.member "SecurityGroupRuleId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.security_group_rule_id (fun f ->
             Aws.Query.Pair ("SecurityGroupRuleId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.security_group_rule_id (fun f ->
             "SecurityGroupRuleId", String.to_json f)
       ])

let of_json j =
  { security_group_rule_id =
      Aws.Util.option_map (Aws.Json.lookup j "SecurityGroupRuleId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
