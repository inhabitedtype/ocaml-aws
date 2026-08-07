open Aws.BaseTypes

type t =
  { user_id : String.t
  ; access_string : String.t option
  ; append_access_string : String.t option
  ; passwords : PasswordListInput.t
  ; no_password_required : Boolean.t option
  ; authentication_mode : AuthenticationMode.t option
  ; engine : String.t option
  }

let make
    ~user_id
    ?access_string
    ?append_access_string
    ?(passwords = [])
    ?no_password_required
    ?authentication_mode
    ?engine
    () =
  { user_id
  ; access_string
  ; append_access_string
  ; passwords
  ; no_password_required
  ; authentication_mode
  ; engine
  }

let parse xml =
  Some
    { user_id =
        Aws.Xml.required
          "UserId"
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
    ; access_string =
        Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse
    ; append_access_string =
        Aws.Util.option_bind (Aws.Xml.member "AppendAccessString" xml) String.parse
    ; passwords =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Passwords" xml) PasswordListInput.parse)
    ; no_password_required =
        Aws.Util.option_bind (Aws.Xml.member "NoPasswordRequired" xml) Boolean.parse
    ; authentication_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthenticationMode" xml)
          AuthenticationMode.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.authentication_mode (fun f ->
             Aws.Query.Pair ("AuthenticationMode", AuthenticationMode.to_query f))
       ; Aws.Util.option_map v.no_password_required (fun f ->
             Aws.Query.Pair ("NoPasswordRequired", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("Passwords.member", PasswordListInput.to_query v.passwords))
       ; Aws.Util.option_map v.append_access_string (fun f ->
             Aws.Query.Pair ("AppendAccessString", String.to_query f))
       ; Aws.Util.option_map v.access_string (fun f ->
             Aws.Query.Pair ("AccessString", String.to_query f))
       ; Some (Aws.Query.Pair ("UserId", String.to_query v.user_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.authentication_mode (fun f ->
             "AuthenticationMode", AuthenticationMode.to_json f)
       ; Aws.Util.option_map v.no_password_required (fun f ->
             "NoPasswordRequired", Boolean.to_json f)
       ; Some ("Passwords", PasswordListInput.to_json v.passwords)
       ; Aws.Util.option_map v.append_access_string (fun f ->
             "AppendAccessString", String.to_json f)
       ; Aws.Util.option_map v.access_string (fun f -> "AccessString", String.to_json f)
       ; Some ("UserId", String.to_json v.user_id)
       ])

let of_json j =
  { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
  ; access_string = Aws.Util.option_map (Aws.Json.lookup j "AccessString") String.of_json
  ; append_access_string =
      Aws.Util.option_map (Aws.Json.lookup j "AppendAccessString") String.of_json
  ; passwords =
      PasswordListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Passwords"))
  ; no_password_required =
      Aws.Util.option_map (Aws.Json.lookup j "NoPasswordRequired") Boolean.of_json
  ; authentication_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "AuthenticationMode")
        AuthenticationMode.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  }
