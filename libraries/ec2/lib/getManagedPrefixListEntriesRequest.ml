open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; prefix_list_id : String.t
  ; target_version : Long.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?dry_run ~prefix_list_id ?target_version ?max_results ?next_token () =
  { dry_run; prefix_list_id; target_version; max_results; next_token }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; prefix_list_id =
        Aws.Xml.required
          "PrefixListId"
          (Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse)
    ; target_version =
        Aws.Util.option_bind (Aws.Xml.member "TargetVersion" xml) Long.parse
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
       ; Aws.Util.option_map v.target_version (fun f ->
             Aws.Query.Pair ("TargetVersion", Long.to_query f))
       ; Some (Aws.Query.Pair ("PrefixListId", String.to_query v.prefix_list_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.target_version (fun f -> "TargetVersion", Long.to_json f)
       ; Some ("PrefixListId", String.to_json v.prefix_list_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; prefix_list_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrefixListId"))
  ; target_version = Aws.Util.option_map (Aws.Json.lookup j "TargetVersion") Long.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
