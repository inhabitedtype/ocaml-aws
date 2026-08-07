open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; architecture_types : ArchitectureTypeSet.t
  ; virtualization_types : VirtualizationTypeSet.t
  ; instance_requirements : InstanceRequirementsRequest.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; context : String.t option
  }

let make
    ?dry_run
    ~architecture_types
    ~virtualization_types
    ~instance_requirements
    ?max_results
    ?next_token
    ?context
    () =
  { dry_run
  ; architecture_types
  ; virtualization_types
  ; instance_requirements
  ; max_results
  ; next_token
  ; context
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; architecture_types =
        Aws.Xml.required
          "ArchitectureType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ArchitectureType" xml)
             ArchitectureTypeSet.parse)
    ; virtualization_types =
        Aws.Xml.required
          "VirtualizationType"
          (Aws.Util.option_bind
             (Aws.Xml.member "VirtualizationType" xml)
             VirtualizationTypeSet.parse)
    ; instance_requirements =
        Aws.Xml.required
          "InstanceRequirements"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceRequirements" xml)
             InstanceRequirementsRequest.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; context = Aws.Util.option_bind (Aws.Xml.member "Context" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceRequirements"
              , InstanceRequirementsRequest.to_query v.instance_requirements ))
       ; Some
           (Aws.Query.Pair
              ("VirtualizationType", VirtualizationTypeSet.to_query v.virtualization_types))
       ; Some
           (Aws.Query.Pair
              ("ArchitectureType", ArchitectureTypeSet.to_query v.architecture_types))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "InstanceRequirements"
           , InstanceRequirementsRequest.to_json v.instance_requirements )
       ; Some ("VirtualizationType", VirtualizationTypeSet.to_json v.virtualization_types)
       ; Some ("ArchitectureType", ArchitectureTypeSet.to_json v.architecture_types)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; architecture_types =
      ArchitectureTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ArchitectureType"))
  ; virtualization_types =
      VirtualizationTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VirtualizationType"))
  ; instance_requirements =
      InstanceRequirementsRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceRequirements"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; context = Aws.Util.option_map (Aws.Json.lookup j "Context") String.of_json
  }
