open Aws.BaseTypes

type t =
  { added_principals : AddedPrincipalSet.t
  ; return_value : Boolean.t option
  }

let make ?(added_principals = []) ?return_value () = { added_principals; return_value }

let parse xml =
  Some
    { added_principals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "addedPrincipalSet" xml)
             AddedPrincipalSet.parse)
    ; return_value = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_value (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AddedPrincipalSet", AddedPrincipalSet.to_query v.added_principals))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_value (fun f -> "return", Boolean.to_json f)
       ; Some ("addedPrincipalSet", AddedPrincipalSet.to_json v.added_principals)
       ])

let of_json j =
  { added_principals =
      AddedPrincipalSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "addedPrincipalSet"))
  ; return_value = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  }
