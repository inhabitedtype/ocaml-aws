open Aws.BaseTypes

type t =
  { parameters : TemplateParameters.t
  ; description : String.t option
  ; capabilities : Capabilities.t
  ; capabilities_reason : String.t option
  ; declared_transforms : TransformsList.t
  }

let make
    ?(parameters = [])
    ?description
    ?(capabilities = [])
    ?capabilities_reason
    ?(declared_transforms = [])
    () =
  { parameters; description; capabilities; capabilities_reason; declared_transforms }

let parse xml =
  Some
    { parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Parameters" xml)
             TemplateParameters.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; capabilities =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
    ; capabilities_reason =
        Aws.Util.option_bind (Aws.Xml.member "CapabilitiesReason" xml) String.parse
    ; declared_transforms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DeclaredTransforms" xml)
             TransformsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DeclaredTransforms.member", TransformsList.to_query v.declared_transforms))
       ; Aws.Util.option_map v.capabilities_reason (fun f ->
             Aws.Query.Pair ("CapabilitiesReason", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Parameters.member", TemplateParameters.to_query v.parameters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DeclaredTransforms", TransformsList.to_json v.declared_transforms)
       ; Aws.Util.option_map v.capabilities_reason (fun f ->
             "CapabilitiesReason", String.to_json f)
       ; Some ("Capabilities", Capabilities.to_json v.capabilities)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("Parameters", TemplateParameters.to_json v.parameters)
       ])

let of_json j =
  { parameters =
      TemplateParameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; capabilities =
      Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
  ; capabilities_reason =
      Aws.Util.option_map (Aws.Json.lookup j "CapabilitiesReason") String.of_json
  ; declared_transforms =
      TransformsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DeclaredTransforms"))
  }
