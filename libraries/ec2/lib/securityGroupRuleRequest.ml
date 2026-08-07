open Aws.BaseTypes

type t =
  { ip_protocol : String.t option
  ; from_port : Integer.t option
  ; to_port : Integer.t option
  ; cidr_ipv4 : String.t option
  ; cidr_ipv6 : String.t option
  ; prefix_list_id : String.t option
  ; referenced_group_id : String.t option
  ; description : String.t option
  }

let make
    ?ip_protocol
    ?from_port
    ?to_port
    ?cidr_ipv4
    ?cidr_ipv6
    ?prefix_list_id
    ?referenced_group_id
    ?description
    () =
  { ip_protocol
  ; from_port
  ; to_port
  ; cidr_ipv4
  ; cidr_ipv6
  ; prefix_list_id
  ; referenced_group_id
  ; description
  }

let parse xml =
  Some
    { ip_protocol = Aws.Util.option_bind (Aws.Xml.member "IpProtocol" xml) String.parse
    ; from_port = Aws.Util.option_bind (Aws.Xml.member "FromPort" xml) Integer.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "ToPort" xml) Integer.parse
    ; cidr_ipv4 = Aws.Util.option_bind (Aws.Xml.member "CidrIpv4" xml) String.parse
    ; cidr_ipv6 = Aws.Util.option_bind (Aws.Xml.member "CidrIpv6" xml) String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "PrefixListId" xml) String.parse
    ; referenced_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReferencedGroupId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.referenced_group_id (fun f ->
             Aws.Query.Pair ("ReferencedGroupId", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.cidr_ipv6 (fun f ->
             Aws.Query.Pair ("CidrIpv6", String.to_query f))
       ; Aws.Util.option_map v.cidr_ipv4 (fun f ->
             Aws.Query.Pair ("CidrIpv4", String.to_query f))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ; Aws.Util.option_map v.ip_protocol (fun f ->
             Aws.Query.Pair ("IpProtocol", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.referenced_group_id (fun f ->
             "ReferencedGroupId", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "PrefixListId", String.to_json f)
       ; Aws.Util.option_map v.cidr_ipv6 (fun f -> "CidrIpv6", String.to_json f)
       ; Aws.Util.option_map v.cidr_ipv4 (fun f -> "CidrIpv4", String.to_json f)
       ; Aws.Util.option_map v.to_port (fun f -> "ToPort", Integer.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "FromPort", Integer.to_json f)
       ; Aws.Util.option_map v.ip_protocol (fun f -> "IpProtocol", String.to_json f)
       ])

let of_json j =
  { ip_protocol = Aws.Util.option_map (Aws.Json.lookup j "IpProtocol") String.of_json
  ; from_port = Aws.Util.option_map (Aws.Json.lookup j "FromPort") Integer.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "ToPort") Integer.of_json
  ; cidr_ipv4 = Aws.Util.option_map (Aws.Json.lookup j "CidrIpv4") String.of_json
  ; cidr_ipv6 = Aws.Util.option_map (Aws.Json.lookup j "CidrIpv6") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "PrefixListId") String.of_json
  ; referenced_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReferencedGroupId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
