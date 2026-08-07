open Aws.BaseTypes

type t =
  { imports : Imports.t
  ; next_token : String.t option
  }

let make ?(imports = []) ?next_token () = { imports; next_token }

let parse xml =
  Some
    { imports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Imports" xml) Imports.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Imports.member", Imports.to_query v.imports))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Imports", Imports.to_json v.imports)
       ])

let of_json j =
  { imports = Imports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Imports"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
