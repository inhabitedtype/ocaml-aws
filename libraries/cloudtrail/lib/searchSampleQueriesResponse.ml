open Aws.BaseTypes

type t =
  { search_results : SearchSampleQueriesSearchResults.t
  ; next_token : String.t option
  }

let make ?(search_results = []) ?next_token () = { search_results; next_token }

let parse xml =
  Some
    { search_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SearchResults" xml)
             SearchSampleQueriesSearchResults.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SearchResults.member"
              , SearchSampleQueriesSearchResults.to_query v.search_results ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("SearchResults", SearchSampleQueriesSearchResults.to_json v.search_results)
       ])

let of_json j =
  { search_results =
      SearchSampleQueriesSearchResults.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SearchResults"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
