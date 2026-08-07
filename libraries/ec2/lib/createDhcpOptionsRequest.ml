open Aws.BaseTypes

type t =
  { dhcp_configurations : NewDhcpConfigurationList.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~dhcp_configurations ?(tag_specifications = []) ?dry_run () =
  { dhcp_configurations; tag_specifications; dry_run }

let parse xml =
  Some
    { dhcp_configurations =
        Aws.Xml.required
          "dhcpConfiguration"
          (Aws.Util.option_bind
             (Aws.Xml.member "dhcpConfiguration" xml)
             NewDhcpConfigurationList.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "DhcpConfiguration"
              , NewDhcpConfigurationList.to_query v.dhcp_configurations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("dhcpConfiguration", NewDhcpConfigurationList.to_json v.dhcp_configurations)
       ])

let of_json j =
  { dhcp_configurations =
      NewDhcpConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "dhcpConfiguration"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
