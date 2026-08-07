open Aws.BaseTypes

type t =
  { keyword : String.t option
  ; settings : StringList.t
  }

let make ?keyword ?(settings = []) () = { keyword; settings }

let parse xml =
  Some
    { keyword = Aws.Util.option_bind (Aws.Xml.member "keyword" xml) String.parse
    ; settings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "settingSet" xml) StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SettingSet", StringList.to_query v.settings))
       ; Aws.Util.option_map v.keyword (fun f ->
             Aws.Query.Pair ("Keyword", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("settingSet", StringList.to_json v.settings)
       ; Aws.Util.option_map v.keyword (fun f -> "keyword", String.to_json f)
       ])

let of_json j =
  { keyword = Aws.Util.option_map (Aws.Json.lookup j "keyword") String.of_json
  ; settings =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "settingSet"))
  }
