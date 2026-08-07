open Aws.BaseTypes

type t =
  { groups : GroupIdentifierList.t
  ; instance_id : String.t option
  ; tags : TagList.t
  ; vpc_id : String.t option
  }

let make ?(groups = []) ?instance_id ?(tags = []) ?vpc_id () =
  { groups; instance_id; tags; vpc_id }

let parse xml =
  Some
    { groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.groups))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Some ("groupSet", GroupIdentifierList.to_json v.groups)
       ])

let of_json j =
  { groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
