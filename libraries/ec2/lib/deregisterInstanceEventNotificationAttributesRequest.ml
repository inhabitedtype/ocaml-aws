open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_tag_attribute : DeregisterInstanceTagAttributeRequest.t
  }

let make ?dry_run ~instance_tag_attribute () = { dry_run; instance_tag_attribute }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_tag_attribute =
        Aws.Xml.required
          "InstanceTagAttribute"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceTagAttribute" xml)
             DeregisterInstanceTagAttributeRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "InstanceTagAttribute"
              , DeregisterInstanceTagAttributeRequest.to_query v.instance_tag_attribute ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "InstanceTagAttribute"
           , DeregisterInstanceTagAttributeRequest.to_json v.instance_tag_attribute )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_tag_attribute =
      DeregisterInstanceTagAttributeRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceTagAttribute"))
  }
