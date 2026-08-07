open Aws.BaseTypes

type t =
  { cidr : String.t
  ; description : String.t option
  }

let make ~cidr ?description () = { cidr; description }

let parse xml =
  Some
    { cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("Cidr", String.to_json v.cidr)
       ])

let of_json j =
  { cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
