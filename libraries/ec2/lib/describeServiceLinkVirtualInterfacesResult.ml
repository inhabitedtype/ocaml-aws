open Aws.BaseTypes

type t =
  { service_link_virtual_interfaces : ServiceLinkVirtualInterfaceSet.t
  ; next_token : String.t option
  }

let make ?(service_link_virtual_interfaces = []) ?next_token () =
  { service_link_virtual_interfaces; next_token }

let parse xml =
  Some
    { service_link_virtual_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceLinkVirtualInterfaceSet" xml)
             ServiceLinkVirtualInterfaceSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ServiceLinkVirtualInterfaceSet"
              , ServiceLinkVirtualInterfaceSet.to_query v.service_link_virtual_interfaces
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "serviceLinkVirtualInterfaceSet"
           , ServiceLinkVirtualInterfaceSet.to_json v.service_link_virtual_interfaces )
       ])

let of_json j =
  { service_link_virtual_interfaces =
      ServiceLinkVirtualInterfaceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceLinkVirtualInterfaceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
