open Aws.BaseTypes

type t =
  { related_resources : RelatedResources.t
  ; next_token : String.t option
  }

let make ?(related_resources = []) ?next_token () = { related_resources; next_token }

let parse xml =
  Some
    { related_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RelatedResources" xml)
             RelatedResources.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RelatedResources.member", RelatedResources.to_query v.related_resources))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("RelatedResources", RelatedResources.to_json v.related_resources)
       ])

let of_json j =
  { related_resources =
      RelatedResources.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RelatedResources"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
