open Aws.BaseTypes

type t =
  { network_interfaces : NetworkInterfaceList.t
  ; next_token : String.t option
  }

let make ?(network_interfaces = []) ?next_token () = { network_interfaces; next_token }

let parse xml =
  Some
    { network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceSet" xml)
             NetworkInterfaceList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("NetworkInterfaceSet", NetworkInterfaceList.to_query v.network_interfaces))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("networkInterfaceSet", NetworkInterfaceList.to_json v.network_interfaces)
       ])

let of_json j =
  { network_interfaces =
      NetworkInterfaceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
