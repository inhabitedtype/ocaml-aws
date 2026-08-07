open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; description : String.t option
  }

let make ?cidr ?description () = { cidr; description }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  }
