open Aws.BaseTypes

type t =
  { cidrs : ValueStringList.t
  ; prefix_list_id : String.t option
  ; prefix_list_name : String.t option
  }

let make ?(cidrs = []) ?prefix_list_id ?prefix_list_name () =
  { cidrs; prefix_list_id; prefix_list_name }

let parse xml =
  Some
    { cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "cidrSet" xml) ValueStringList.parse)
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; prefix_list_name =
        Aws.Util.option_bind (Aws.Xml.member "prefixListName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix_list_name (fun f ->
             Aws.Query.Pair ("PrefixListName", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Some (Aws.Query.Pair ("CidrSet", ValueStringList.to_query v.cidrs))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prefix_list_name (fun f ->
             "prefixListName", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Some ("cidrSet", ValueStringList.to_json v.cidrs)
       ])

let of_json j =
  { cidrs = ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrSet"))
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; prefix_list_name =
      Aws.Util.option_map (Aws.Json.lookup j "prefixListName") String.of_json
  }
