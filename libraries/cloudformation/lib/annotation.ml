open Aws.BaseTypes

type t =
  { annotation_name : String.t option
  ; status : AnnotationStatus.t option
  ; status_message : String.t option
  ; remediation_message : String.t option
  ; remediation_link : String.t option
  ; severity_level : AnnotationSeverityLevel.t option
  }

let make
    ?annotation_name
    ?status
    ?status_message
    ?remediation_message
    ?remediation_link
    ?severity_level
    () =
  { annotation_name
  ; status
  ; status_message
  ; remediation_message
  ; remediation_link
  ; severity_level
  }

let parse xml =
  Some
    { annotation_name =
        Aws.Util.option_bind (Aws.Xml.member "AnnotationName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) AnnotationStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
    ; remediation_message =
        Aws.Util.option_bind (Aws.Xml.member "RemediationMessage" xml) String.parse
    ; remediation_link =
        Aws.Util.option_bind (Aws.Xml.member "RemediationLink" xml) String.parse
    ; severity_level =
        Aws.Util.option_bind
          (Aws.Xml.member "SeverityLevel" xml)
          AnnotationSeverityLevel.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.severity_level (fun f ->
             Aws.Query.Pair ("SeverityLevel", AnnotationSeverityLevel.to_query f))
       ; Aws.Util.option_map v.remediation_link (fun f ->
             Aws.Query.Pair ("RemediationLink", String.to_query f))
       ; Aws.Util.option_map v.remediation_message (fun f ->
             Aws.Query.Pair ("RemediationMessage", String.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AnnotationStatus.to_query f))
       ; Aws.Util.option_map v.annotation_name (fun f ->
             Aws.Query.Pair ("AnnotationName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.severity_level (fun f ->
             "SeverityLevel", AnnotationSeverityLevel.to_json f)
       ; Aws.Util.option_map v.remediation_link (fun f ->
             "RemediationLink", String.to_json f)
       ; Aws.Util.option_map v.remediation_message (fun f ->
             "RemediationMessage", String.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "StatusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", AnnotationStatus.to_json f)
       ; Aws.Util.option_map v.annotation_name (fun f ->
             "AnnotationName", String.to_json f)
       ])

let of_json j =
  { annotation_name =
      Aws.Util.option_map (Aws.Json.lookup j "AnnotationName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") AnnotationStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
  ; remediation_message =
      Aws.Util.option_map (Aws.Json.lookup j "RemediationMessage") String.of_json
  ; remediation_link =
      Aws.Util.option_map (Aws.Json.lookup j "RemediationLink") String.of_json
  ; severity_level =
      Aws.Util.option_map
        (Aws.Json.lookup j "SeverityLevel")
        AnnotationSeverityLevel.of_json
  }
