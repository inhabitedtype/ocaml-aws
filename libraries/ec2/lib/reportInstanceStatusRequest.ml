open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instances : InstanceIdStringList.t
  ; status : ReportStatusType.t
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; reason_codes : ReasonCodesList.t
  ; description : String.t option
  }

let make ?dry_run ~instances ~status ?start_time ?end_time ~reason_codes ?description () =
  { dry_run; instances; status; start_time; end_time; reason_codes; description }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; instances =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceId" xml)
             InstanceIdStringList.parse)
    ; status =
        Aws.Xml.required
          "status"
          (Aws.Util.option_bind (Aws.Xml.member "status" xml) ReportStatusType.parse)
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "endTime" xml) DateTime.parse
    ; reason_codes =
        Aws.Xml.required
          "reasonCode"
          (Aws.Util.option_bind (Aws.Xml.member "reasonCode" xml) ReasonCodesList.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("ReasonCode", ReasonCodesList.to_query v.reason_codes))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("Status", ReportStatusType.to_query v.status))
       ; Some (Aws.Query.Pair ("InstanceId", InstanceIdStringList.to_query v.instances))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("reasonCode", ReasonCodesList.to_json v.reason_codes)
       ; Aws.Util.option_map v.end_time (fun f -> "endTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Some ("status", ReportStatusType.to_json v.status)
       ; Some ("instanceId", InstanceIdStringList.to_json v.instances)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; instances =
      InstanceIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; status =
      ReportStatusType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "status"))
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "endTime") DateTime.of_json
  ; reason_codes =
      ReasonCodesList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "reasonCode"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  }
