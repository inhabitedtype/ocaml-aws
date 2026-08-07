open Aws.BaseTypes

type t =
  { rule_name : String.t
  ; state : String.t
  }

let make ~rule_name ~state () = { rule_name; state }

let parse xml =
  Some
    { rule_name =
        Aws.Xml.required
          "RuleName"
          (Aws.Util.option_bind (Aws.Xml.member "RuleName" xml) String.parse)
    ; state =
        Aws.Xml.required
          "State"
          (Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("State", String.to_query v.state))
       ; Some (Aws.Query.Pair ("RuleName", String.to_query v.rule_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("State", String.to_json v.state)
       ; Some ("RuleName", String.to_json v.rule_name)
       ])

let of_json j =
  { rule_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleName"))
  ; state = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "State"))
  }
