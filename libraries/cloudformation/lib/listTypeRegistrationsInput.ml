open Aws.BaseTypes

type t =
  { type_ : RegistryType.t option
  ; type_name : String.t option
  ; type_arn : String.t option
  ; registration_status_filter : RegistrationStatus.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?type_
    ?type_name
    ?type_arn
    ?registration_status_filter
    ?max_results
    ?next_token
    () =
  { type_; type_name; type_arn; registration_status_filter; max_results; next_token }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
    ; registration_status_filter =
        Aws.Util.option_bind
          (Aws.Xml.member "RegistrationStatusFilter" xml)
          RegistrationStatus.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.registration_status_filter (fun f ->
             Aws.Query.Pair ("RegistrationStatusFilter", RegistrationStatus.to_query f))
       ; Aws.Util.option_map v.type_arn (fun f ->
             Aws.Query.Pair ("TypeArn", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.registration_status_filter (fun f ->
             "RegistrationStatusFilter", RegistrationStatus.to_json f)
       ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
  ; registration_status_filter =
      Aws.Util.option_map
        (Aws.Json.lookup j "RegistrationStatusFilter")
        RegistrationStatus.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
