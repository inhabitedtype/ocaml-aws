open Aws.BaseTypes

type t =
  { ipv6_cidr : String.t option
  ; associated_resource : String.t option
  }

let make ?ipv6_cidr ?associated_resource () = { ipv6_cidr; associated_resource }

let parse xml =
  Some
    { ipv6_cidr = Aws.Util.option_bind (Aws.Xml.member "ipv6Cidr" xml) String.parse
    ; associated_resource =
        Aws.Util.option_bind (Aws.Xml.member "associatedResource" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associated_resource (fun f ->
             Aws.Query.Pair ("AssociatedResource", String.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr (fun f ->
             Aws.Query.Pair ("Ipv6Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.associated_resource (fun f ->
             "associatedResource", String.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr (fun f -> "ipv6Cidr", String.to_json f)
       ])

let of_json j =
  { ipv6_cidr = Aws.Util.option_map (Aws.Json.lookup j "ipv6Cidr") String.of_json
  ; associated_resource =
      Aws.Util.option_map (Aws.Json.lookup j "associatedResource") String.of_json
  }
