open Aws.BaseTypes

type t =
  { results_count : Integer.t option
  ; total_results_count : Integer.t option
  ; bytes_scanned : Long.t option
  }

let make ?results_count ?total_results_count ?bytes_scanned () =
  { results_count; total_results_count; bytes_scanned }

let parse xml =
  Some
    { results_count =
        Aws.Util.option_bind (Aws.Xml.member "ResultsCount" xml) Integer.parse
    ; total_results_count =
        Aws.Util.option_bind (Aws.Xml.member "TotalResultsCount" xml) Integer.parse
    ; bytes_scanned = Aws.Util.option_bind (Aws.Xml.member "BytesScanned" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bytes_scanned (fun f ->
             Aws.Query.Pair ("BytesScanned", Long.to_query f))
       ; Aws.Util.option_map v.total_results_count (fun f ->
             Aws.Query.Pair ("TotalResultsCount", Integer.to_query f))
       ; Aws.Util.option_map v.results_count (fun f ->
             Aws.Query.Pair ("ResultsCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bytes_scanned (fun f -> "BytesScanned", Long.to_json f)
       ; Aws.Util.option_map v.total_results_count (fun f ->
             "TotalResultsCount", Integer.to_json f)
       ; Aws.Util.option_map v.results_count (fun f -> "ResultsCount", Integer.to_json f)
       ])

let of_json j =
  { results_count = Aws.Util.option_map (Aws.Json.lookup j "ResultsCount") Integer.of_json
  ; total_results_count =
      Aws.Util.option_map (Aws.Json.lookup j "TotalResultsCount") Integer.of_json
  ; bytes_scanned = Aws.Util.option_map (Aws.Json.lookup j "BytesScanned") Long.of_json
  }
