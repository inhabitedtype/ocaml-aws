open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; resource_type_options : ImageUsageResourceTypeOptionRequestList.t
  }

let make ?resource_type ?(resource_type_options = []) () =
  { resource_type; resource_type_options }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; resource_type_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceTypeOption" xml)
             ImageUsageResourceTypeOptionRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ResourceTypeOption"
              , ImageUsageResourceTypeOptionRequestList.to_query v.resource_type_options
              ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ResourceTypeOption"
           , ImageUsageResourceTypeOptionRequestList.to_json v.resource_type_options )
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; resource_type_options =
      ImageUsageResourceTypeOptionRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypeOption"))
  }
