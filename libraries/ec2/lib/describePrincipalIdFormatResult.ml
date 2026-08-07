open Aws.BaseTypes

type t =
  { principals : PrincipalIdFormatList.t
  ; next_token : String.t option
  }

let make ?(principals = []) ?next_token () = { principals; next_token }

let parse xml =
  Some
    { principals =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "principalSet" xml)
             PrincipalIdFormatList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("PrincipalSet", PrincipalIdFormatList.to_query v.principals))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("principalSet", PrincipalIdFormatList.to_json v.principals)
       ])

let of_json j =
  { principals =
      PrincipalIdFormatList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "principalSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
