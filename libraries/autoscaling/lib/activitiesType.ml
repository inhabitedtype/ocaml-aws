open Aws.BaseTypes

type t =
  { activities : Activities.t
  ; next_token : String.t option
  }

let make ~activities ?next_token () = { activities; next_token }

let parse xml =
  Some
    { activities =
        Aws.Xml.required
          "Activities"
          (Aws.Util.option_bind (Aws.Xml.member "Activities" xml) Activities.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Activities.member", Activities.to_query v.activities))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Activities", Activities.to_json v.activities)
       ])

let of_json j =
  { activities =
      Activities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Activities"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
