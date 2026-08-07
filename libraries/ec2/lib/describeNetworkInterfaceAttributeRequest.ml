open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; network_interface_id : String.t
  ; attribute : NetworkInterfaceAttribute.t option
  }

let make ?dry_run ~network_interface_id ?attribute () =
  { dry_run; network_interface_id; attribute }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; attribute =
        Aws.Util.option_bind
          (Aws.Xml.member "attribute" xml)
          NetworkInterfaceAttribute.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", NetworkInterfaceAttribute.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute (fun f ->
             "attribute", NetworkInterfaceAttribute.to_json f)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; attribute =
      Aws.Util.option_map
        (Aws.Json.lookup j "attribute")
        NetworkInterfaceAttribute.of_json
  }
