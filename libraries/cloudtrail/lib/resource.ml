open Aws.BaseTypes

type t =
  { resource_type : String.t option
  ; resource_name : String.t option
  }

let make ?resource_type ?resource_name () = { resource_type; resource_name }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; resource_name =
        Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_name (fun f ->
             Aws.Query.Pair ("ResourceName", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_name (fun f -> "ResourceName", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ])

let of_json j =
  { resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; resource_name = Aws.Util.option_map (Aws.Json.lookup j "ResourceName") String.of_json
  }
