open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; insight_rules : InsightRules.t
  }

let make ?next_token ?(insight_rules = []) () = { next_token; insight_rules }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; insight_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InsightRules" xml) InsightRules.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("InsightRules.member", InsightRules.to_query v.insight_rules))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("InsightRules", InsightRules.to_json v.insight_rules)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; insight_rules =
      InsightRules.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightRules"))
  }
