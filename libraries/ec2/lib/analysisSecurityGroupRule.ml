open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; direction : String.t option
  ; security_group_id : String.t option
  ; port_range : PortRange.t option
  ; prefix_list_id : String.t option
  ; protocol : String.t option
  }

let make ?cidr ?direction ?security_group_id ?port_range ?prefix_list_id ?protocol () =
  { cidr; direction; security_group_id; port_range; prefix_list_id; protocol }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; direction = Aws.Util.option_bind (Aws.Xml.member "direction" xml) String.parse
    ; security_group_id =
        Aws.Util.option_bind (Aws.Xml.member "securityGroupId" xml) String.parse
    ; port_range = Aws.Util.option_bind (Aws.Xml.member "portRange" xml) PortRange.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.port_range (fun f ->
             Aws.Query.Pair ("PortRange", PortRange.to_query f))
       ; Aws.Util.option_map v.security_group_id (fun f ->
             Aws.Query.Pair ("SecurityGroupId", String.to_query f))
       ; Aws.Util.option_map v.direction (fun f ->
             Aws.Query.Pair ("Direction", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.port_range (fun f -> "portRange", PortRange.to_json f)
       ; Aws.Util.option_map v.security_group_id (fun f ->
             "securityGroupId", String.to_json f)
       ; Aws.Util.option_map v.direction (fun f -> "direction", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; direction = Aws.Util.option_map (Aws.Json.lookup j "direction") String.of_json
  ; security_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "securityGroupId") String.of_json
  ; port_range = Aws.Util.option_map (Aws.Json.lookup j "portRange") PortRange.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  }
