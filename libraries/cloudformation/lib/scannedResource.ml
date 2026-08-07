open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; resource_identifier : JazzResourceIdentifierProperties.t option
  ; managed_by_stack : Boolean.t option
  }

let make ?resource_type ?resource_identifier ?managed_by_stack () =
  { resource_type; resource_identifier; managed_by_stack }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; resource_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceIdentifier" xml)
          JazzResourceIdentifierProperties.parse
    ; managed_by_stack =
        Aws.Util.option_bind (Aws.Xml.member "ManagedByStack" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by_stack (fun f ->
             Aws.Query.Pair ("ManagedByStack", Boolean.to_query f))
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             Aws.Query.Pair
               ("ResourceIdentifier", JazzResourceIdentifierProperties.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by_stack (fun f ->
             "ManagedByStack", Boolean.to_json f)
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             "ResourceIdentifier", JazzResourceIdentifierProperties.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; resource_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceIdentifier")
        JazzResourceIdentifierProperties.of_json
  ; managed_by_stack =
      Aws.Util.option_map (Aws.Json.lookup j "ManagedByStack") Boolean.of_json
  }
