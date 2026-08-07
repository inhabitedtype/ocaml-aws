open Aws.BaseTypes

type t =
  { progress_status : RegistrationStatus.t option
  ; description : String.t option
  ; type_arn : String.t option
  ; type_version_arn : String.t option
  }

let make ?progress_status ?description ?type_arn ?type_version_arn () =
  { progress_status; description; type_arn; type_version_arn }

let parse xml =
  Some
    { progress_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ProgressStatus" xml)
          RegistrationStatus.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; type_version_arn =
        Aws.Util.option_bind (Aws.Xml.member "TypeVersionArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_version_arn (fun f ->
             Aws.Query.Pair ("TypeVersionArn", String.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.progress_status (fun f ->
             Aws.Query.Pair ("ProgressStatus", RegistrationStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_version_arn (fun f ->
             "TypeVersionArn", String.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.progress_status (fun f ->
             "ProgressStatus", RegistrationStatus.to_json f)
       ])

let of_json j =
  { progress_status =
      Aws.Util.option_map (Aws.Json.lookup j "ProgressStatus") RegistrationStatus.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; type_version_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TypeVersionArn") String.of_json
  }
