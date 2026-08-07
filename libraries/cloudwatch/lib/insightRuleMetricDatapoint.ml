open Aws.BaseTypes

type t =
  { timestamp : DateTime.t
  ; unique_contributors : Double.t option
  ; max_contributor_value : Double.t option
  ; sample_count : Double.t option
  ; average : Double.t option
  ; sum : Double.t option
  ; minimum : Double.t option
  ; maximum : Double.t option
  }

let make
    ~timestamp
    ?unique_contributors
    ?max_contributor_value
    ?sample_count
    ?average
    ?sum
    ?minimum
    ?maximum
    () =
  { timestamp
  ; unique_contributors
  ; max_contributor_value
  ; sample_count
  ; average
  ; sum
  ; minimum
  ; maximum
  }

let parse xml =
  Some
    { timestamp =
        Aws.Xml.required
          "Timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
    ; unique_contributors =
        Aws.Util.option_bind (Aws.Xml.member "UniqueContributors" xml) Double.parse
    ; max_contributor_value =
        Aws.Util.option_bind (Aws.Xml.member "MaxContributorValue" xml) Double.parse
    ; sample_count = Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse
    ; average = Aws.Util.option_bind (Aws.Xml.member "Average" xml) Double.parse
    ; sum = Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse
    ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse
    ; maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum (fun f ->
             Aws.Query.Pair ("Maximum", Double.to_query f))
       ; Aws.Util.option_map v.minimum (fun f ->
             Aws.Query.Pair ("Minimum", Double.to_query f))
       ; Aws.Util.option_map v.sum (fun f -> Aws.Query.Pair ("Sum", Double.to_query f))
       ; Aws.Util.option_map v.average (fun f ->
             Aws.Query.Pair ("Average", Double.to_query f))
       ; Aws.Util.option_map v.sample_count (fun f ->
             Aws.Query.Pair ("SampleCount", Double.to_query f))
       ; Aws.Util.option_map v.max_contributor_value (fun f ->
             Aws.Query.Pair ("MaxContributorValue", Double.to_query f))
       ; Aws.Util.option_map v.unique_contributors (fun f ->
             Aws.Query.Pair ("UniqueContributors", Double.to_query f))
       ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum (fun f -> "Maximum", Double.to_json f)
       ; Aws.Util.option_map v.minimum (fun f -> "Minimum", Double.to_json f)
       ; Aws.Util.option_map v.sum (fun f -> "Sum", Double.to_json f)
       ; Aws.Util.option_map v.average (fun f -> "Average", Double.to_json f)
       ; Aws.Util.option_map v.sample_count (fun f -> "SampleCount", Double.to_json f)
       ; Aws.Util.option_map v.max_contributor_value (fun f ->
             "MaxContributorValue", Double.to_json f)
       ; Aws.Util.option_map v.unique_contributors (fun f ->
             "UniqueContributors", Double.to_json f)
       ; Some ("Timestamp", DateTime.to_json v.timestamp)
       ])

let of_json j =
  { timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
  ; unique_contributors =
      Aws.Util.option_map (Aws.Json.lookup j "UniqueContributors") Double.of_json
  ; max_contributor_value =
      Aws.Util.option_map (Aws.Json.lookup j "MaxContributorValue") Double.of_json
  ; sample_count = Aws.Util.option_map (Aws.Json.lookup j "SampleCount") Double.of_json
  ; average = Aws.Util.option_map (Aws.Json.lookup j "Average") Double.of_json
  ; sum = Aws.Util.option_map (Aws.Json.lookup j "Sum") Double.of_json
  ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Double.of_json
  ; maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Double.of_json
  }
