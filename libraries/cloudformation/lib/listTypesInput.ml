open Aws.BaseTypes

type t =
  { visibility : Visibility.t option
  ; provisioning_type : ProvisioningType.t option
  ; deprecated_status : DeprecatedStatus.t option
  ; type_ : RegistryType.t option
  ; filters : TypeFilters.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?visibility
    ?provisioning_type
    ?deprecated_status
    ?type_
    ?filters
    ?max_results
    ?next_token
    () =
  { visibility
  ; provisioning_type
  ; deprecated_status
  ; type_
  ; filters
  ; max_results
  ; next_token
  }

let parse xml =
  Some
    { visibility = Aws.Util.option_bind (Aws.Xml.member "Visibility" xml) Visibility.parse
    ; provisioning_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ProvisioningType" xml)
          ProvisioningType.parse
    ; deprecated_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DeprecatedStatus" xml)
          DeprecatedStatus.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
    ; filters = Aws.Util.option_bind (Aws.Xml.member "Filters" xml) TypeFilters.parse
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
       ; Aws.Util.option_map v.filters (fun f ->
             Aws.Query.Pair ("Filters", TypeFilters.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", RegistryType.to_query f))
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
       ; Aws.Util.option_map v.provisioning_type (fun f ->
             Aws.Query.Pair ("ProvisioningType", ProvisioningType.to_query f))
       ; Aws.Util.option_map v.visibility (fun f ->
             Aws.Query.Pair ("Visibility", Visibility.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.filters (fun f -> "Filters", TypeFilters.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
       ; Aws.Util.option_map v.deprecated_status (fun f ->
             "DeprecatedStatus", DeprecatedStatus.to_json f)
       ; Aws.Util.option_map v.provisioning_type (fun f ->
             "ProvisioningType", ProvisioningType.to_json f)
       ; Aws.Util.option_map v.visibility (fun f -> "Visibility", Visibility.to_json f)
       ])

let of_json j =
  { visibility = Aws.Util.option_map (Aws.Json.lookup j "Visibility") Visibility.of_json
  ; provisioning_type =
      Aws.Util.option_map (Aws.Json.lookup j "ProvisioningType") ProvisioningType.of_json
  ; deprecated_status =
      Aws.Util.option_map (Aws.Json.lookup j "DeprecatedStatus") DeprecatedStatus.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
  ; filters = Aws.Util.option_map (Aws.Json.lookup j "Filters") TypeFilters.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
