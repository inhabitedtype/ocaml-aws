open Aws.BaseTypes

type t =
  { queries : Queries.t
  ; next_token : String.t option
  }

let make ?(queries = []) ?next_token () = { queries; next_token }

let parse xml =
  Some
    { queries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Queries" xml) Queries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Queries.member", Queries.to_query v.queries))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Queries", Queries.to_json v.queries)
       ])

let of_json j =
  { queries = Queries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Queries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
