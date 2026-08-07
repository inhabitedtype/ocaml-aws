open Aws.BaseTypes

type t =
  { from_port : Integer.t option
  ; ip_protocol : String.t option
  ; ip_ranges : IpRanges.t
  ; prefix_list_ids : PrefixListIdSet.t
  ; to_port : Integer.t option
  ; user_id_group_pairs : UserIdGroupPairSet.t
  }

let make
    ?from_port
    ?ip_protocol
    ?(ip_ranges = [])
    ?(prefix_list_ids = [])
    ?to_port
    ?(user_id_group_pairs = [])
    () =
  { from_port; ip_protocol; ip_ranges; prefix_list_ids; to_port; user_id_group_pairs }

let parse xml =
  Some
    { from_port = Aws.Util.option_bind (Aws.Xml.member "fromPort" xml) Integer.parse
    ; ip_protocol = Aws.Util.option_bind (Aws.Xml.member "ipProtocol" xml) String.parse
    ; ip_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipRanges" xml) IpRanges.parse)
    ; prefix_list_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "prefixListIds" xml)
             PrefixListIdSet.parse)
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "toPort" xml) Integer.parse
    ; user_id_group_pairs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groups" xml) UserIdGroupPairSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Groups", UserIdGroupPairSet.to_query v.user_id_group_pairs))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("PrefixListIds", PrefixListIdSet.to_query v.prefix_list_ids))
       ; Some (Aws.Query.Pair ("IpRanges", IpRanges.to_query v.ip_ranges))
       ; Aws.Util.option_map v.ip_protocol (fun f ->
             Aws.Query.Pair ("IpProtocol", String.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("groups", UserIdGroupPairSet.to_json v.user_id_group_pairs)
       ; Aws.Util.option_map v.to_port (fun f -> "toPort", Integer.to_json f)
       ; Some ("prefixListIds", PrefixListIdSet.to_json v.prefix_list_ids)
       ; Some ("ipRanges", IpRanges.to_json v.ip_ranges)
       ; Aws.Util.option_map v.ip_protocol (fun f -> "ipProtocol", String.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "fromPort", Integer.to_json f)
       ])

let of_json j =
  { from_port = Aws.Util.option_map (Aws.Json.lookup j "fromPort") Integer.of_json
  ; ip_protocol = Aws.Util.option_map (Aws.Json.lookup j "ipProtocol") String.of_json
  ; ip_ranges = IpRanges.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipRanges"))
  ; prefix_list_ids =
      PrefixListIdSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "prefixListIds"))
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "toPort") Integer.of_json
  ; user_id_group_pairs =
      UserIdGroupPairSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groups"))
  }
