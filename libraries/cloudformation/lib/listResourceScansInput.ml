open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; max_results : Integer.t option
  ; scan_type_filter : ScanType.t option
  }

let make ?next_token ?max_results ?scan_type_filter () =
  { next_token; max_results; scan_type_filter }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; scan_type_filter =
        Aws.Util.option_bind (Aws.Xml.member "ScanTypeFilter" xml) ScanType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scan_type_filter (fun f ->
             Aws.Query.Pair ("ScanTypeFilter", ScanType.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scan_type_filter (fun f ->
             "ScanTypeFilter", ScanType.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; scan_type_filter =
      Aws.Util.option_map (Aws.Json.lookup j "ScanTypeFilter") ScanType.of_json
  }
