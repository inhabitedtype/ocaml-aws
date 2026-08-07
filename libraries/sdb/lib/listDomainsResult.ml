open Aws.BaseTypes

type t =
  { domain_names : DomainNameList.t
  ; next_token : String.t option
  }

let make ?(domain_names = []) ?next_token () = { domain_names; next_token }

let parse xml =
  Some
    { domain_names = Aws.Util.of_option [] (DomainNameList.parse xml)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("DomainNames.member", DomainNameList.to_query v.domain_names))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("DomainNames", DomainNameList.to_json v.domain_names)
       ])

let of_json j =
  { domain_names =
      DomainNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainNames"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
