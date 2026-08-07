type t = { port_ranges : ModifyVerifiedAccessEndpointPortRangeList.t }

let make ?(port_ranges = []) () = { port_ranges }

let parse xml =
  Some
    { port_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PortRange" xml)
             ModifyVerifiedAccessEndpointPortRangeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PortRange"
              , ModifyVerifiedAccessEndpointPortRangeList.to_query v.port_ranges ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("PortRange", ModifyVerifiedAccessEndpointPortRangeList.to_json v.port_ranges)
       ])

let of_json j =
  { port_ranges =
      ModifyVerifiedAccessEndpointPortRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PortRange"))
  }
