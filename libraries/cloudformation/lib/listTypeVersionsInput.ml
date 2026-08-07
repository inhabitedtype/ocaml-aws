open Aws.BaseTypes

type t =
  { type_ : RegistryType.t option
  ; type_name : String.t option
  ; arn : String.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; deprecated_status : DeprecatedStatus.t option
  ; publisher_id : String.t option
  }

let make
    ?type_
    ?type_name
    ?arn
    ?max_results
    ?next_token
    ?deprecated_status
    ?publisher_id
    () =
  { type_; type_name; arn; max_results; next_token; deprecated_status; publisher_id }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; deprecated_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DeprecatedStatus" xml)
          DeprecatedStatus.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             "DeprecatedStatus", DeprecatedStatus.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; deprecated_status =
      Aws.Util.option_map (Aws.Json.lookup j "DeprecatedStatus") DeprecatedStatus.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  }
