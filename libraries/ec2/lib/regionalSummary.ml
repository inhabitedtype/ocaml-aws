open Aws.BaseTypes

type t =
  { region_name : String.t option
  ; number_of_matched_accounts : Integer.t option
  ; number_of_unmatched_accounts : Integer.t option
  }

let make ?region_name ?number_of_matched_accounts ?number_of_unmatched_accounts () =
  { region_name; number_of_matched_accounts; number_of_unmatched_accounts }

let parse xml =
  Some
    { region_name = Aws.Util.option_bind (Aws.Xml.member "regionName" xml) String.parse
    ; number_of_matched_accounts =
        Aws.Util.option_bind (Aws.Xml.member "numberOfMatchedAccounts" xml) Integer.parse
    ; number_of_unmatched_accounts =
        Aws.Util.option_bind
          (Aws.Xml.member "numberOfUnmatchedAccounts" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_unmatched_accounts (fun f ->
             Aws.Query.Pair ("NumberOfUnmatchedAccounts", Integer.to_query f))
       ; Aws.Util.option_map v.number_of_matched_accounts (fun f ->
             Aws.Query.Pair ("NumberOfMatchedAccounts", Integer.to_query f))
       ; Aws.Util.option_map v.region_name (fun f ->
             Aws.Query.Pair ("RegionName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_unmatched_accounts (fun f ->
             "numberOfUnmatchedAccounts", Integer.to_json f)
       ; Aws.Util.option_map v.number_of_matched_accounts (fun f ->
             "numberOfMatchedAccounts", Integer.to_json f)
       ; Aws.Util.option_map v.region_name (fun f -> "regionName", String.to_json f)
       ])

let of_json j =
  { region_name = Aws.Util.option_map (Aws.Json.lookup j "regionName") String.of_json
  ; number_of_matched_accounts =
      Aws.Util.option_map (Aws.Json.lookup j "numberOfMatchedAccounts") Integer.of_json
  ; number_of_unmatched_accounts =
      Aws.Util.option_map (Aws.Json.lookup j "numberOfUnmatchedAccounts") Integer.of_json
  }
