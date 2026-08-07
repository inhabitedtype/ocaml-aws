open Aws.BaseTypes

type t =
  { resource_type : TagResourceType.t option
  ; resource_id : String.t option
  ; tags : TagList.t
  }

let make ?resource_type ?resource_id ?(tags = []) () =
  { resource_type; resource_id; tags }

let parse xml =
  Some
    { resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) TagResourceType.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", TagResourceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", TagResourceType.to_json f)
       ])

let of_json j =
  { resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceType") TagResourceType.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
