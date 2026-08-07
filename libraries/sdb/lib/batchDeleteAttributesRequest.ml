open Aws.BaseTypes

type t =
  { domain_name : String.t
  ; items : DeletableItemList.t
  }

let make ~domain_name ~items () = { domain_name; items }

let parse xml =
  Some
    { domain_name =
        Aws.Xml.required
          "DomainName"
          (Aws.Util.option_bind (Aws.Xml.member "DomainName" xml) String.parse)
    ; items = Aws.Xml.required "Items" (DeletableItemList.parse xml)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Items.member", DeletableItemList.to_query v.items))
       ; Some (Aws.Query.Pair ("DomainName", String.to_query v.domain_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Items", DeletableItemList.to_json v.items)
       ; Some ("DomainName", String.to_json v.domain_name)
       ])

let of_json j =
  { domain_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainName"))
  ; items = DeletableItemList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Items"))
  }
