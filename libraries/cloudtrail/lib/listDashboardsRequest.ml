open Aws.BaseTypes

type t =
  { name_prefix : String.t option
  ; type_ : DashboardType.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ?name_prefix ?type_ ?next_token ?max_results () =
  { name_prefix; type_; next_token; max_results }

let parse xml =
  Some
    { name_prefix = Aws.Util.option_bind (Aws.Xml.member "NamePrefix" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) DashboardType.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", DashboardType.to_query f))
       ; Aws.Util.option_map v.name_prefix (fun f ->
             Aws.Query.Pair ("NamePrefix", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", DashboardType.to_json f)
       ; Aws.Util.option_map v.name_prefix (fun f -> "NamePrefix", String.to_json f)
       ])

let of_json j =
  { name_prefix = Aws.Util.option_map (Aws.Json.lookup j "NamePrefix") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") DashboardType.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
