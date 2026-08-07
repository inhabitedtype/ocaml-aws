open Aws.BaseTypes

type t =
  { access_key_id : String.t
  ; secret_access_key : String.t
  ; session_token : String.t
  ; expiration : DateTime.t
  }

let make ~access_key_id ~secret_access_key ~session_token ~expiration () =
  { access_key_id; secret_access_key; session_token; expiration }

let parse xml =
  Some
    { access_key_id =
        Aws.Xml.required
          "AccessKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse)
    ; secret_access_key =
        Aws.Xml.required
          "SecretAccessKey"
          (Aws.Util.option_bind (Aws.Xml.member "SecretAccessKey" xml) String.parse)
    ; session_token =
        Aws.Xml.required
          "SessionToken"
          (Aws.Util.option_bind (Aws.Xml.member "SessionToken" xml) String.parse)
    ; expiration =
        Aws.Xml.required
          "Expiration"
          (Aws.Util.option_bind (Aws.Xml.member "Expiration" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Expiration", DateTime.to_query v.expiration))
       ; Some (Aws.Query.Pair ("SessionToken", String.to_query v.session_token))
       ; Some (Aws.Query.Pair ("SecretAccessKey", String.to_query v.secret_access_key))
       ; Some (Aws.Query.Pair ("AccessKeyId", String.to_query v.access_key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Expiration", DateTime.to_json v.expiration)
       ; Some ("SessionToken", String.to_json v.session_token)
       ; Some ("SecretAccessKey", String.to_json v.secret_access_key)
       ; Some ("AccessKeyId", String.to_json v.access_key_id)
       ])

let of_json j =
  { access_key_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessKeyId"))
  ; secret_access_key =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecretAccessKey"))
  ; session_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SessionToken"))
  ; expiration =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Expiration"))
  }
