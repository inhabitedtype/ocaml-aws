open Aws.BaseTypes

type t =
  { source_arn : String.t
  ; max_results : Integer.t option
  }

let make ~source_arn ?max_results () = { source_arn; max_results }

let parse xml =
  Some
    { source_arn =
        Aws.Xml.required
          "SourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SourceArn", String.to_query v.source_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("SourceArn", String.to_json v.source_arn)
       ])

let of_json j =
  { source_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceArn"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
