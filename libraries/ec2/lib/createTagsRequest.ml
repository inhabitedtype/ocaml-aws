open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; resources : ResourceIdList.t
  ; tags : TagList.t
  }

let make ?dry_run ~resources ~tags () = { dry_run; resources; tags }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; resources =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) ResourceIdList.parse)
    ; tags =
        Aws.Xml.required
          "Tag"
          (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tag", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("ResourceId", ResourceIdList.to_query v.resources))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tag", TagList.to_json v.tags)
       ; Some ("ResourceId", ResourceIdList.to_json v.resources)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; resources =
      ResourceIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag"))
  }
