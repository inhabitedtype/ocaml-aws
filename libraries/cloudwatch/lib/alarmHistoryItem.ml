open Aws.BaseTypes

type t =
  { alarm_name : String.t option
  ; alarm_contributor_id : String.t option
  ; alarm_type : AlarmType.t option
  ; timestamp : DateTime.t option
  ; history_item_type : HistoryItemType.t option
  ; history_summary : String.t option
  ; history_data : String.t option
  ; alarm_contributor_attributes : ContributorAttributes.t option
  }

let make
    ?alarm_name
    ?alarm_contributor_id
    ?alarm_type
    ?timestamp
    ?history_item_type
    ?history_summary
    ?history_data
    ?alarm_contributor_attributes
    () =
  { alarm_name
  ; alarm_contributor_id
  ; alarm_type
  ; timestamp
  ; history_item_type
  ; history_summary
  ; history_data
  ; alarm_contributor_attributes
  }

let parse xml =
  Some
    { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; alarm_contributor_id =
        Aws.Util.option_bind (Aws.Xml.member "AlarmContributorId" xml) String.parse
    ; alarm_type = Aws.Util.option_bind (Aws.Xml.member "AlarmType" xml) AlarmType.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
    ; history_item_type =
        Aws.Util.option_bind (Aws.Xml.member "HistoryItemType" xml) HistoryItemType.parse
    ; history_summary =
        Aws.Util.option_bind (Aws.Xml.member "HistorySummary" xml) String.parse
    ; history_data = Aws.Util.option_bind (Aws.Xml.member "HistoryData" xml) String.parse
    ; alarm_contributor_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "AlarmContributorAttributes" xml)
          ContributorAttributes.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alarm_contributor_attributes (fun f ->
             Aws.Query.Pair
               ("AlarmContributorAttributes", ContributorAttributes.to_query f))
       ; Aws.Util.option_map v.history_data (fun f ->
             Aws.Query.Pair ("HistoryData", String.to_query f))
       ; Aws.Util.option_map v.history_summary (fun f ->
             Aws.Query.Pair ("HistorySummary", String.to_query f))
       ; Aws.Util.option_map v.history_item_type (fun f ->
             Aws.Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.alarm_type (fun f ->
             Aws.Query.Pair ("AlarmType", AlarmType.to_query f))
       ; Aws.Util.option_map v.alarm_contributor_id (fun f ->
             Aws.Query.Pair ("AlarmContributorId", String.to_query f))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.alarm_contributor_attributes (fun f ->
             "AlarmContributorAttributes", ContributorAttributes.to_json f)
       ; Aws.Util.option_map v.history_data (fun f -> "HistoryData", String.to_json f)
       ; Aws.Util.option_map v.history_summary (fun f ->
             "HistorySummary", String.to_json f)
       ; Aws.Util.option_map v.history_item_type (fun f ->
             "HistoryItemType", HistoryItemType.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.alarm_type (fun f -> "AlarmType", AlarmType.to_json f)
       ; Aws.Util.option_map v.alarm_contributor_id (fun f ->
             "AlarmContributorId", String.to_json f)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ])

let of_json j =
  { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; alarm_contributor_id =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmContributorId") String.of_json
  ; alarm_type = Aws.Util.option_map (Aws.Json.lookup j "AlarmType") AlarmType.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
  ; history_item_type =
      Aws.Util.option_map (Aws.Json.lookup j "HistoryItemType") HistoryItemType.of_json
  ; history_summary =
      Aws.Util.option_map (Aws.Json.lookup j "HistorySummary") String.of_json
  ; history_data = Aws.Util.option_map (Aws.Json.lookup j "HistoryData") String.of_json
  ; alarm_contributor_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "AlarmContributorAttributes")
        ContributorAttributes.of_json
  }
