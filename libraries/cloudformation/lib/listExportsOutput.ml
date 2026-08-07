open Aws.BaseTypes

type t =
  { exports : Exports.t
  ; next_token : String.t option
  }

let make ?(exports = []) ?next_token () = { exports; next_token }

let parse xml =
  Some
    { exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Exports" xml) Exports.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Exports.member", Exports.to_query v.exports))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Exports", Exports.to_json v.exports)
       ])

let of_json j =
  { exports = Exports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Exports"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
