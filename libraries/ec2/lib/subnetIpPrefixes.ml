open Aws.BaseTypes

type t =
  { subnet_id : String.t option
  ; ip_prefixes : ValueStringList.t
  }

let make ?subnet_id ?(ip_prefixes = []) () = { subnet_id; ip_prefixes }

let parse xml =
  Some
    { subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; ip_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipPrefixSet" xml) ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IpPrefixSet", ValueStringList.to_query v.ip_prefixes))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipPrefixSet", ValueStringList.to_json v.ip_prefixes)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ])

let of_json j =
  { subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; ip_prefixes =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipPrefixSet"))
  }
