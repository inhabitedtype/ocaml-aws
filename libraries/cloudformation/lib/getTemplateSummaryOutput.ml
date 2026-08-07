open Aws.BaseTypes

type t =
  { parameters : ParameterDeclarations.t
  ; description : String.t option
  ; capabilities : Capabilities.t
  ; capabilities_reason : String.t option
  ; resource_types : ResourceTypes.t
  ; version : String.t option
  ; metadata : String.t option
  ; declared_transforms : TransformsList.t
  ; resource_identifier_summaries : ResourceIdentifierSummaries.t
  ; warnings : Warnings.t option
  }

let make
    ?(parameters = [])
    ?description
    ?(capabilities = [])
    ?capabilities_reason
    ?(resource_types = [])
    ?version
    ?metadata
    ?(declared_transforms = [])
    ?(resource_identifier_summaries = [])
    ?warnings
    () =
  { parameters
  ; description
  ; capabilities
  ; capabilities_reason
  ; resource_types
  ; version
  ; metadata
  ; declared_transforms
  ; resource_identifier_summaries
  ; warnings
  }

let parse xml =
  Some
    { parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Parameters" xml)
             ParameterDeclarations.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; capabilities_reason =
        Aws.Util.option_bind (Aws.Xml.member "CapabilitiesReason" xml) String.parse
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ResourceTypes" xml) ResourceTypes.parse)
    ; version = Aws.Util.option_bind (Aws.Xml.member "Version" xml) String.parse
    ; metadata = Aws.Util.option_bind (Aws.Xml.member "Metadata" xml) String.parse
    ; declared_transforms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DeclaredTransforms" xml)
             TransformsList.parse)
    ; resource_identifier_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceIdentifierSummaries" xml)
             ResourceIdentifierSummaries.parse)
    ; warnings = Aws.Util.option_bind (Aws.Xml.member "Warnings" xml) Warnings.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warnings (fun f ->
             Aws.Query.Pair ("Warnings", Warnings.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ResourceIdentifierSummaries.member"
              , ResourceIdentifierSummaries.to_query v.resource_identifier_summaries ))
       ; Some
           (Aws.Query.Pair
              ("DeclaredTransforms.member", TransformsList.to_query v.declared_transforms))
       ; Aws.Util.option_map v.metadata (fun f ->
             Aws.Query.Pair ("Metadata", String.to_query f))
       ; Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
       ; Aws.Util.option_map v.capabilities_reason (fun f ->
             Aws.Query.Pair ("CapabilitiesReason", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Parameters.member", ParameterDeclarations.to_query v.parameters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warnings (fun f -> "Warnings", Warnings.to_json f)
       ; Some
           ( "ResourceIdentifierSummaries"
           , ResourceIdentifierSummaries.to_json v.resource_identifier_summaries )
       ; Some ("DeclaredTransforms", TransformsList.to_json v.declared_transforms)
       ; Aws.Util.option_map v.metadata (fun f -> "Metadata", String.to_json f)
       ; Aws.Util.option_map v.version (fun f -> "Version", String.to_json f)
       ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
       ; Aws.Util.option_map v.capabilities_reason (fun f ->
             "CapabilitiesReason", String.to_json f)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("Parameters", ParameterDeclarations.to_json v.parameters)
       ])

let of_json j =
  { parameters =
      ParameterDeclarations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; capabilities_reason =
      Aws.Util.option_map (Aws.Json.lookup j "CapabilitiesReason") String.of_json
  ; resource_types =
      ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
  ; version = Aws.Util.option_map (Aws.Json.lookup j "Version") String.of_json
  ; metadata = Aws.Util.option_map (Aws.Json.lookup j "Metadata") String.of_json
  ; declared_transforms =
      TransformsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DeclaredTransforms"))
  ; resource_identifier_summaries =
      ResourceIdentifierSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifierSummaries"))
  ; warnings = Aws.Util.option_map (Aws.Json.lookup j "Warnings") Warnings.of_json
  }
