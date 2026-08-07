open Aws.BaseTypes

type t =
  { image_ids : DescribeImageUsageReportsImageIdStringList.t
  ; report_ids : ImageUsageReportIdStringList.t
  ; next_token : String.t option
  ; filters : FilterList.t
  ; dry_run : Boolean.t option
  ; max_results : Integer.t option
  }

let make
    ?(image_ids = [])
    ?(report_ids = [])
    ?next_token
    ?(filters = [])
    ?dry_run
    ?max_results
    () =
  { image_ids; report_ids; next_token; filters; dry_run; max_results }

let parse xml =
  Some
    { image_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageId" xml)
             DescribeImageUsageReportsImageIdStringList.parse)
    ; report_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReportId" xml)
             ImageUsageReportIdStringList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ReportId", ImageUsageReportIdStringList.to_query v.report_ids))
       ; Some
           (Aws.Query.Pair
              ("ImageId", DescribeImageUsageReportsImageIdStringList.to_query v.image_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ReportId", ImageUsageReportIdStringList.to_json v.report_ids)
       ; Some ("ImageId", DescribeImageUsageReportsImageIdStringList.to_json v.image_ids)
       ])

let of_json j =
  { image_ids =
      DescribeImageUsageReportsImageIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; report_ids =
      ImageUsageReportIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReportId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
