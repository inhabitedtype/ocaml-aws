open Aws.BaseTypes

type t =
  { attribute_name : String.t option
  ; most_frequent_value : String.t option
  ; number_of_matched_accounts : Integer.t option
  ; number_of_unmatched_accounts : Integer.t option
  ; regional_summaries : RegionalSummaryList.t
  }

let make
    ?attribute_name
    ?most_frequent_value
    ?number_of_matched_accounts
    ?number_of_unmatched_accounts
    ?(regional_summaries = [])
    () =
  { attribute_name
  ; most_frequent_value
  ; number_of_matched_accounts
  ; number_of_unmatched_accounts
  ; regional_summaries
  }

let parse xml =
  Some
    { attribute_name =
        Aws.Util.option_bind (Aws.Xml.member "attributeName" xml) String.parse
    ; most_frequent_value =
        Aws.Util.option_bind (Aws.Xml.member "mostFrequentValue" xml) String.parse
    ; number_of_matched_accounts =
        Aws.Util.option_bind (Aws.Xml.member "numberOfMatchedAccounts" xml) Integer.parse
    ; number_of_unmatched_accounts =
        Aws.Util.option_bind
          (Aws.Xml.member "numberOfUnmatchedAccounts" xml)
          Integer.parse
    ; regional_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "regionalSummarySet" xml)
             RegionalSummaryList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("RegionalSummarySet", RegionalSummaryList.to_query v.regional_summaries))
       ; Aws.Util.option_map v.number_of_unmatched_accounts (fun f ->
             Aws.Query.Pair ("NumberOfUnmatchedAccounts", Integer.to_query f))
       ; Aws.Util.option_map v.number_of_matched_accounts (fun f ->
             Aws.Query.Pair ("NumberOfMatchedAccounts", Integer.to_query f))
       ; Aws.Util.option_map v.most_frequent_value (fun f ->
             Aws.Query.Pair ("MostFrequentValue", String.to_query f))
       ; Aws.Util.option_map v.attribute_name (fun f ->
             Aws.Query.Pair ("AttributeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("regionalSummarySet", RegionalSummaryList.to_json v.regional_summaries)
       ; Aws.Util.option_map v.number_of_unmatched_accounts (fun f ->
             "numberOfUnmatchedAccounts", Integer.to_json f)
       ; Aws.Util.option_map v.number_of_matched_accounts (fun f ->
             "numberOfMatchedAccounts", Integer.to_json f)
       ; Aws.Util.option_map v.most_frequent_value (fun f ->
             "mostFrequentValue", String.to_json f)
       ; Aws.Util.option_map v.attribute_name (fun f -> "attributeName", String.to_json f)
       ])

let of_json j =
  { attribute_name =
      Aws.Util.option_map (Aws.Json.lookup j "attributeName") String.of_json
  ; most_frequent_value =
      Aws.Util.option_map (Aws.Json.lookup j "mostFrequentValue") String.of_json
  ; number_of_matched_accounts =
      Aws.Util.option_map (Aws.Json.lookup j "numberOfMatchedAccounts") Integer.of_json
  ; number_of_unmatched_accounts =
      Aws.Util.option_map (Aws.Json.lookup j "numberOfUnmatchedAccounts") Integer.of_json
  ; regional_summaries =
      RegionalSummaryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "regionalSummarySet"))
  }
