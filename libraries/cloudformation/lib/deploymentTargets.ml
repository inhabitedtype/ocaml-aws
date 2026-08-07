open Aws.BaseTypes

type t =
  { accounts : AccountList.t
  ; accounts_url : String.t option
  ; organizational_unit_ids : OrganizationalUnitIdList.t
  ; account_filter_type : AccountFilterType.t option
  }

let make
    ?(accounts = [])
    ?accounts_url
    ?(organizational_unit_ids = [])
    ?account_filter_type
    () =
  { accounts; accounts_url; organizational_unit_ids; account_filter_type }

let parse xml =
  Some
    { accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
    ; accounts_url = Aws.Util.option_bind (Aws.Xml.member "AccountsUrl" xml) String.parse
    ; organizational_unit_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OrganizationalUnitIds" xml)
             OrganizationalUnitIdList.parse)
    ; account_filter_type =
        Aws.Util.option_bind
          (Aws.Xml.member "AccountFilterType" xml)
          AccountFilterType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_filter_type (fun f ->
             Aws.Query.Pair ("AccountFilterType", AccountFilterType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OrganizationalUnitIds.member"
              , OrganizationalUnitIdList.to_query v.organizational_unit_ids ))
       ; Aws.Util.option_map v.accounts_url (fun f ->
             Aws.Query.Pair ("AccountsUrl", String.to_query f))
       ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account_filter_type (fun f ->
             "AccountFilterType", AccountFilterType.to_json f)
       ; Some
           ( "OrganizationalUnitIds"
           , OrganizationalUnitIdList.to_json v.organizational_unit_ids )
       ; Aws.Util.option_map v.accounts_url (fun f -> "AccountsUrl", String.to_json f)
       ; Some ("Accounts", AccountList.to_json v.accounts)
       ])

let of_json j =
  { accounts = AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
  ; accounts_url = Aws.Util.option_map (Aws.Json.lookup j "AccountsUrl") String.of_json
  ; organizational_unit_ids =
      OrganizationalUnitIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationalUnitIds"))
  ; account_filter_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "AccountFilterType")
        AccountFilterType.of_json
  }
