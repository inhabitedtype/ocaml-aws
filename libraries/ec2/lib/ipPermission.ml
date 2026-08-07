open Aws.BaseTypes

type t =
  { ip_protocol : String.t
  ; from_port : Integer.t option
  ; to_port : Integer.t option
  ; user_id_group_pairs : UserIdGroupPairList.t
  ; ip_ranges : IpRangeList.t
  ; ipv6_ranges : Ipv6RangeList.t
  ; prefix_list_ids : PrefixListIdList.t
  }

let make
    ~ip_protocol
    ?from_port
    ?to_port
    ?(user_id_group_pairs = [])
    ?(ip_ranges = [])
    ?(ipv6_ranges = [])
    ?(prefix_list_ids = [])
    () =
  { ip_protocol
  ; from_port
  ; to_port
  ; user_id_group_pairs
  ; ip_ranges
  ; ipv6_ranges
  ; prefix_list_ids
  }

let parse xml =
  Some
    { ip_protocol =
        Aws.Xml.required
          "ipProtocol"
          (Aws.Util.option_bind (Aws.Xml.member "ipProtocol" xml) String.parse)
    ; from_port = Aws.Util.option_bind (Aws.Xml.member "fromPort" xml) Integer.parse
    ; to_port = Aws.Util.option_bind (Aws.Xml.member "toPort" xml) Integer.parse
    ; user_id_group_pairs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groups" xml) UserIdGroupPairList.parse)
    ; ip_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipRanges" xml) IpRangeList.parse)
    ; ipv6_ranges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipv6Ranges" xml) Ipv6RangeList.parse)
    ; prefix_list_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "prefixListIds" xml)
             PrefixListIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("PrefixListIds", PrefixListIdList.to_query v.prefix_list_ids))
       ; Some (Aws.Query.Pair ("Ipv6Ranges", Ipv6RangeList.to_query v.ipv6_ranges))
       ; Some (Aws.Query.Pair ("IpRanges", IpRangeList.to_query v.ip_ranges))
       ; Some
           (Aws.Query.Pair ("Groups", UserIdGroupPairList.to_query v.user_id_group_pairs))
       ; Aws.Util.option_map v.to_port (fun f ->
             Aws.Query.Pair ("ToPort", Integer.to_query f))
       ; Aws.Util.option_map v.from_port (fun f ->
             Aws.Query.Pair ("FromPort", Integer.to_query f))
       ; Some (Aws.Query.Pair ("IpProtocol", String.to_query v.ip_protocol))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("prefixListIds", PrefixListIdList.to_json v.prefix_list_ids)
       ; Some ("ipv6Ranges", Ipv6RangeList.to_json v.ipv6_ranges)
       ; Some ("ipRanges", IpRangeList.to_json v.ip_ranges)
       ; Some ("groups", UserIdGroupPairList.to_json v.user_id_group_pairs)
       ; Aws.Util.option_map v.to_port (fun f -> "toPort", Integer.to_json f)
       ; Aws.Util.option_map v.from_port (fun f -> "fromPort", Integer.to_json f)
       ; Some ("ipProtocol", String.to_json v.ip_protocol)
       ])

let of_json j =
  { ip_protocol = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipProtocol"))
  ; from_port = Aws.Util.option_map (Aws.Json.lookup j "fromPort") Integer.of_json
  ; to_port = Aws.Util.option_map (Aws.Json.lookup j "toPort") Integer.of_json
  ; user_id_group_pairs =
      UserIdGroupPairList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groups"))
  ; ip_ranges =
      IpRangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipRanges"))
  ; ipv6_ranges =
      Ipv6RangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6Ranges"))
  ; prefix_list_ids =
      PrefixListIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "prefixListIds"))
  }
