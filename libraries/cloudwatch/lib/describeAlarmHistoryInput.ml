open Aws.BaseTypes

type t =
  { alarm_name : String.t option
  ; alarm_contributor_id : String.t option
  ; alarm_types : AlarmTypes.t
  ; history_item_type : HistoryItemType.t option
  ; start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; max_records : Integer.t option
  ; next_token : String.t option
  ; scan_by : ScanBy.t option
  }

let make
    ?alarm_name
    ?alarm_contributor_id
    ?(alarm_types = [])
    ?history_item_type
    ?start_date
    ?end_date
    ?max_records
    ?next_token
    ?scan_by
    () =
  { alarm_name
  ; alarm_contributor_id
  ; alarm_types
  ; history_item_type
  ; start_date
  ; end_date
  ; max_records
  ; next_token
  ; scan_by
  }

let parse xml =
  Some
    { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; alarm_contributor_id =
        Aws.Util.option_bind (Aws.Xml.member "AlarmContributorId" xml) String.parse
    ; alarm_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AlarmTypes" xml) AlarmTypes.parse)
    ; history_item_type =
        Aws.Util.option_bind (Aws.Xml.member "HistoryItemType" xml) HistoryItemType.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "StartDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; scan_by = Aws.Util.option_bind (Aws.Xml.member "ScanBy" xml) ScanBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scan_by (fun f ->
             Aws.Query.Pair ("ScanBy", ScanBy.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.history_item_type (fun f ->
             Aws.Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
       ; Some (Aws.Query.Pair ("AlarmTypes.member", AlarmTypes.to_query v.alarm_types))
       ; Aws.Util.option_map v.alarm_contributor_id (fun f ->
             Aws.Query.Pair ("AlarmContributorId", String.to_query f))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scan_by (fun f -> "ScanBy", ScanBy.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "StartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.history_item_type (fun f ->
             "HistoryItemType", HistoryItemType.to_json f)
       ; Some ("AlarmTypes", AlarmTypes.to_json v.alarm_types)
       ; Aws.Util.option_map v.alarm_contributor_id (fun f ->
             "AlarmContributorId", String.to_json f)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ])

let of_json j =
  { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; alarm_contributor_id =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmContributorId") String.of_json
  ; alarm_types =
      AlarmTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmTypes"))
  ; history_item_type =
      Aws.Util.option_map (Aws.Json.lookup j "HistoryItemType") HistoryItemType.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "StartDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; scan_by = Aws.Util.option_map (Aws.Json.lookup j "ScanBy") ScanBy.of_json
  }
