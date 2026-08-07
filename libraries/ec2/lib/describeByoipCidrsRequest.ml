open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; max_results : Integer.t
  ; next_token : String.t option
  }

let make ?dry_run ~max_results ?next_token () = { dry_run; max_results; next_token }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; max_results =
        Aws.Xml.required
          "MaxResults"
          (Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("MaxResults", Integer.to_query v.max_results))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("MaxResults", Integer.to_json v.max_results)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; max_results =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxResults"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
