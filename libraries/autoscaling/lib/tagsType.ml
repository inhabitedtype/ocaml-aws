open Aws.BaseTypes

type t =
  { tags : TagDescriptionList.t
  ; next_token : String.t option
  }

let make ?(tags = []) ?next_token () = { tags; next_token }

let parse xml =
  Some
    { tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagDescriptionList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagDescriptionList.to_query v.tags))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Tags", TagDescriptionList.to_json v.tags)
       ])

let of_json j =
  { tags = TagDescriptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
