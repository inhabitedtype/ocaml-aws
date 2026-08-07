type t = { instance_tag_attribute : InstanceTagNotificationAttribute.t option }

let make ?instance_tag_attribute () = { instance_tag_attribute }

let parse xml =
  Some
    { instance_tag_attribute =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceTagAttribute" xml)
          InstanceTagNotificationAttribute.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_tag_attribute (fun f ->
             Aws.Query.Pair
               ("InstanceTagAttribute", InstanceTagNotificationAttribute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_tag_attribute (fun f ->
             "instanceTagAttribute", InstanceTagNotificationAttribute.to_json f)
       ])

let of_json j =
  { instance_tag_attribute =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceTagAttribute")
        InstanceTagNotificationAttribute.of_json
  }
