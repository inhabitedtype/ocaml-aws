open Aws.BaseTypes

type t =
  { classic_link_enabled : Boolean.t option
  ; tags : TagList.t
  ; vpc_id : String.t option
  }

let make ?classic_link_enabled ?(tags = []) ?vpc_id () =
  { classic_link_enabled; tags; vpc_id }

let parse xml =
  Some
    { classic_link_enabled =
        Aws.Util.option_bind (Aws.Xml.member "classicLinkEnabled" xml) Boolean.parse
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
       ; Aws.Util.option_map v.classic_link_enabled (fun f ->
             Aws.Query.Pair ("ClassicLinkEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.classic_link_enabled (fun f ->
             "classicLinkEnabled", Boolean.to_json f)
       ])

let of_json j =
  { classic_link_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "classicLinkEnabled") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
