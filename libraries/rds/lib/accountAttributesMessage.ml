type t = { account_quotas : AccountQuotaList.t }

let make ?(account_quotas = []) () = { account_quotas }

let parse xml =
  Some
    { account_quotas =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AccountQuotas" xml)
             AccountQuotaList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AccountQuotas.member", AccountQuotaList.to_query v.account_quotas))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AccountQuotas", AccountQuotaList.to_json v.account_quotas) ])

let of_json j =
  { account_quotas =
      AccountQuotaList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AccountQuotas"))
  }
