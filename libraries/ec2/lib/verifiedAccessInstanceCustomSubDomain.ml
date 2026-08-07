open Aws.BaseTypes

type t =
  { sub_domain : String.t option
  ; nameservers : ValueStringList.t
  }

let make ?sub_domain ?(nameservers = []) () = { sub_domain; nameservers }

let parse xml =
  Some
    { sub_domain = Aws.Util.option_bind (Aws.Xml.member "subDomain" xml) String.parse
    ; nameservers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "nameserverSet" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("NameserverSet", ValueStringList.to_query v.nameservers))
       ; Aws.Util.option_map v.sub_domain (fun f ->
             Aws.Query.Pair ("SubDomain", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("nameserverSet", ValueStringList.to_json v.nameservers)
       ; Aws.Util.option_map v.sub_domain (fun f -> "subDomain", String.to_json f)
       ])

let of_json j =
  { sub_domain = Aws.Util.option_map (Aws.Json.lookup j "subDomain") String.of_json
  ; nameservers =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "nameserverSet"))
  }
