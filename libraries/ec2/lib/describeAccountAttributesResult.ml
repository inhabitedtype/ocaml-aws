type t = { account_attributes : AccountAttributeList.t }

let make ?(account_attributes = []) () = { account_attributes }

let parse xml =
  Some
    { account_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "accountAttributeSet" xml)
             AccountAttributeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AccountAttributeSet", AccountAttributeList.to_query v.account_attributes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("accountAttributeSet", AccountAttributeList.to_json v.account_attributes) ])

let of_json j =
  { account_attributes =
      AccountAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "accountAttributeSet"))
  }
