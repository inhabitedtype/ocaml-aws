open Aws.BaseTypes

type t =
  { description : String.t option
  ; enable_stack_creation : Boolean.t option
  ; resource_mappings : ResourceMappings.t
  ; stack_definitions : StackDefinitions.t
  }

let make
    ?description
    ?enable_stack_creation
    ?(resource_mappings = [])
    ~stack_definitions
    () =
  { description; enable_stack_creation; resource_mappings; stack_definitions }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; enable_stack_creation =
        Aws.Util.option_bind (Aws.Xml.member "EnableStackCreation" xml) Boolean.parse
    ; resource_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceMappings" xml)
             ResourceMappings.parse)
    ; stack_definitions =
        Aws.Xml.required
          "StackDefinitions"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackDefinitions" xml)
             StackDefinitions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("StackDefinitions.member", StackDefinitions.to_query v.stack_definitions))
       ; Some
           (Aws.Query.Pair
              ("ResourceMappings.member", ResourceMappings.to_query v.resource_mappings))
       ; Aws.Util.option_map v.enable_stack_creation (fun f ->
             Aws.Query.Pair ("EnableStackCreation", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackDefinitions", StackDefinitions.to_json v.stack_definitions)
       ; Some ("ResourceMappings", ResourceMappings.to_json v.resource_mappings)
       ; Aws.Util.option_map v.enable_stack_creation (fun f ->
             "EnableStackCreation", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; enable_stack_creation =
      Aws.Util.option_map (Aws.Json.lookup j "EnableStackCreation") Boolean.of_json
  ; resource_mappings =
      ResourceMappings.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceMappings"))
  ; stack_definitions =
      StackDefinitions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDefinitions"))
  }
