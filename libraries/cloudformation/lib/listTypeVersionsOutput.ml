open Aws.BaseTypes

type t =
  { type_version_summaries : TypeVersionSummaries.t
  ; next_token : String.t option
  }

let make ?(type_version_summaries = []) ?next_token () =
  { type_version_summaries; next_token }

let parse xml =
  Some
    { type_version_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TypeVersionSummaries" xml)
             TypeVersionSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TypeVersionSummaries.member"
              , TypeVersionSummaries.to_query v.type_version_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ("TypeVersionSummaries", TypeVersionSummaries.to_json v.type_version_summaries)
       ])

let of_json j =
  { type_version_summaries =
      TypeVersionSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeVersionSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
