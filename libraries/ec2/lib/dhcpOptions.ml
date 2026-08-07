open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; tags : TagList.t
  ; dhcp_options_id : String.t option
  ; dhcp_configurations : DhcpConfigurationList.t
  }

let make ?owner_id ?(tags = []) ?dhcp_options_id ?(dhcp_configurations = []) () =
  { owner_id; tags; dhcp_options_id; dhcp_configurations }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; dhcp_options_id =
        Aws.Util.option_bind (Aws.Xml.member "dhcpOptionsId" xml) String.parse
    ; dhcp_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "dhcpConfigurationSet" xml)
             DhcpConfigurationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DhcpConfigurationSet"
              , DhcpConfigurationList.to_query v.dhcp_configurations ))
       ; Aws.Util.option_map v.dhcp_options_id (fun f ->
             Aws.Query.Pair ("DhcpOptionsId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("dhcpConfigurationSet", DhcpConfigurationList.to_json v.dhcp_configurations)
       ; Aws.Util.option_map v.dhcp_options_id (fun f ->
             "dhcpOptionsId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; dhcp_options_id =
      Aws.Util.option_map (Aws.Json.lookup j "dhcpOptionsId") String.of_json
  ; dhcp_configurations =
      DhcpConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "dhcpConfigurationSet"))
  }
