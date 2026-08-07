open Aws.BaseTypes

type t =
  { account_quota_name : String.t option
  ; used : Long.t option
  ; max : Long.t option
  }

let make ?account_quota_name ?used ?max () = { account_quota_name; used; max }

let parse xml =
  Some
    { account_quota_name =
        Aws.Util.option_bind (Aws.Xml.member "AccountQuotaName" xml) String.parse
    ; used = Aws.Util.option_bind (Aws.Xml.member "Used" xml) Long.parse
    ; max = Aws.Util.option_bind (Aws.Xml.member "Max" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> Aws.Query.Pair ("Max", Long.to_query f))
       ; Aws.Util.option_map v.used (fun f -> Aws.Query.Pair ("Used", Long.to_query f))
       ; Aws.Util.option_map v.account_quota_name (fun f ->
             Aws.Query.Pair ("AccountQuotaName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max (fun f -> "Max", Long.to_json f)
       ; Aws.Util.option_map v.used (fun f -> "Used", Long.to_json f)
       ; Aws.Util.option_map v.account_quota_name (fun f ->
             "AccountQuotaName", String.to_json f)
       ])

let of_json j =
  { account_quota_name =
      Aws.Util.option_map (Aws.Json.lookup j "AccountQuotaName") String.of_json
  ; used = Aws.Util.option_map (Aws.Json.lookup j "Used") Long.of_json
  ; max = Aws.Util.option_map (Aws.Json.lookup j "Max") Long.of_json
  }
