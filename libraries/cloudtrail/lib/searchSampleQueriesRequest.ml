open Aws.BaseTypes

type t =
  { search_phrase : String.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ~search_phrase ?max_results ?next_token () =
  { search_phrase; max_results; next_token }

let parse xml =
  Some
    { search_phrase =
        Aws.Xml.required
          "SearchPhrase"
          (Aws.Util.option_bind (Aws.Xml.member "SearchPhrase" xml) String.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SearchPhrase", String.to_query v.search_phrase))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("SearchPhrase", String.to_json v.search_phrase)
       ])

let of_json j =
  { search_phrase =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SearchPhrase"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
