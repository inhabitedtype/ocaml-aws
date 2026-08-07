open Aws.BaseTypes

type t =
  { user_id : String.t
  ; user_name : String.t
  ; engine : String.t
  ; passwords : PasswordListInput.t
  ; access_string : String.t
  ; no_password_required : Boolean.t option
  ; tags : TagList.t
  ; authentication_mode : AuthenticationMode.t option
  }

let make
    ~user_id
    ~user_name
    ~engine
    ?(passwords = [])
    ~access_string
    ?no_password_required
    ?(tags = [])
    ?authentication_mode
    () =
  { user_id
  ; user_name
  ; engine
  ; passwords
  ; access_string
  ; no_password_required
  ; tags
  ; authentication_mode
  }

let parse xml =
  Some
    { user_id =
        Aws.Xml.required
          "UserId"
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
    ; user_name =
        Aws.Xml.required
          "UserName"
          (Aws.Util.option_bind (Aws.Xml.member "UserName" xml) String.parse)
    ; engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; passwords =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Passwords" xml) PasswordListInput.parse)
    ; access_string =
        Aws.Xml.required
          "AccessString"
          (Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse)
    ; no_password_required =
        Aws.Util.option_bind (Aws.Xml.member "NoPasswordRequired" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; authentication_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthenticationMode" xml)
          AuthenticationMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.authentication_mode (fun f ->
             Aws.Query.Pair ("AuthenticationMode", AuthenticationMode.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.no_password_required (fun f ->
             Aws.Query.Pair ("NoPasswordRequired", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("AccessString", String.to_query v.access_string))
       ; Some
           (Aws.Query.Pair ("Passwords.member", PasswordListInput.to_query v.passwords))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ; Some (Aws.Query.Pair ("UserName", String.to_query v.user_name))
       ; Some (Aws.Query.Pair ("UserId", String.to_query v.user_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.authentication_mode (fun f ->
             "AuthenticationMode", AuthenticationMode.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.no_password_required (fun f ->
             "NoPasswordRequired", Boolean.to_json f)
       ; Some ("AccessString", String.to_json v.access_string)
       ; Some ("Passwords", PasswordListInput.to_json v.passwords)
       ; Some ("Engine", String.to_json v.engine)
       ; Some ("UserName", String.to_json v.user_name)
       ; Some ("UserId", String.to_json v.user_id)
       ])

let of_json j =
  { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
  ; user_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserName"))
  ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; passwords =
      PasswordListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Passwords"))
  ; access_string =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessString"))
  ; no_password_required =
      Aws.Util.option_map (Aws.Json.lookup j "NoPasswordRequired") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; authentication_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "AuthenticationMode")
        AuthenticationMode.of_json
  }
