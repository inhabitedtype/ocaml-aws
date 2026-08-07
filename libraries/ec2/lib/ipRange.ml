open Aws.BaseTypes

type t =
  { description : String.t option
  ; cidr_ip : String.t
  }

let make ?description ~cidr_ip () = { description; cidr_ip }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; cidr_ip =
        Aws.Xml.required
          "cidrIp"
          (Aws.Util.option_bind (Aws.Xml.member "cidrIp" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CidrIp", String.to_query v.cidr_ip))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("cidrIp", String.to_json v.cidr_ip)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; cidr_ip = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrIp"))
  }
