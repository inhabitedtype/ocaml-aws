open Aws.BaseTypes

type t =
  { instance_tag_keys : InstanceTagKeySet.t
  ; include_all_tags_of_instance : Boolean.t option
  }

let make ?(instance_tag_keys = []) ?include_all_tags_of_instance () =
  { instance_tag_keys; include_all_tags_of_instance }

let parse xml =
  Some
    { instance_tag_keys =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceTagKeySet" xml)
             InstanceTagKeySet.parse)
    ; include_all_tags_of_instance =
        Aws.Util.option_bind (Aws.Xml.member "includeAllTagsOfInstance" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all_tags_of_instance (fun f ->
             Aws.Query.Pair ("IncludeAllTagsOfInstance", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InstanceTagKeySet", InstanceTagKeySet.to_query v.instance_tag_keys))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all_tags_of_instance (fun f ->
             "includeAllTagsOfInstance", Boolean.to_json f)
       ; Some ("instanceTagKeySet", InstanceTagKeySet.to_json v.instance_tag_keys)
       ])

let of_json j =
  { instance_tag_keys =
      InstanceTagKeySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceTagKeySet"))
  ; include_all_tags_of_instance =
      Aws.Util.option_map (Aws.Json.lookup j "includeAllTagsOfInstance") Boolean.of_json
  }
