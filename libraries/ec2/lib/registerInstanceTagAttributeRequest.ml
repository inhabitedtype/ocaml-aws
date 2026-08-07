open Aws.BaseTypes

type t =
  { include_all_tags_of_instance : Boolean.t option
  ; instance_tag_keys : InstanceTagKeySet.t
  }

let make ?include_all_tags_of_instance ?(instance_tag_keys = []) () =
  { include_all_tags_of_instance; instance_tag_keys }

let parse xml =
  Some
    { include_all_tags_of_instance =
        Aws.Util.option_bind (Aws.Xml.member "IncludeAllTagsOfInstance" xml) Boolean.parse
    ; instance_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceTagKey" xml)
             InstanceTagKeySet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("InstanceTagKey", InstanceTagKeySet.to_query v.instance_tag_keys))
       ; Aws.Util.option_map v.include_all_tags_of_instance (fun f ->
             Aws.Query.Pair ("IncludeAllTagsOfInstance", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("InstanceTagKey", InstanceTagKeySet.to_json v.instance_tag_keys)
       ; Aws.Util.option_map v.include_all_tags_of_instance (fun f ->
             "IncludeAllTagsOfInstance", Boolean.to_json f)
       ])

let of_json j =
  { include_all_tags_of_instance =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeAllTagsOfInstance") Boolean.of_json
  ; instance_tag_keys =
      InstanceTagKeySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceTagKey"))
  }
