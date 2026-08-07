open Aws.BaseTypes

type t =
  { data_responses : DataResponses.t
  ; next_token : String.t option
  }

let make ?(data_responses = []) ?next_token () = { data_responses; next_token }

let parse xml =
  Some
    { data_responses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "dataResponseSet" xml)
             DataResponses.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("DataResponseSet", DataResponses.to_query v.data_responses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("dataResponseSet", DataResponses.to_json v.data_responses)
       ])

let of_json j =
  { data_responses =
      DataResponses.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "dataResponseSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
