module Hash = struct
  let _sha256 ?key str =
    match key with
    | Some key -> Digestif.SHA256.hmac_string ~key str
    | None -> Digestif.SHA256.digest_string str

  let sha256 ?key str = _sha256 ?key str |> Digestif.SHA256.to_raw_string

  let sha256_hex ?key str = _sha256 ?key str |> Digestif.SHA256.to_hex

  let sha256_base64 ?key str = Base64.encode_string @@ sha256 ?key str
end

let encode_query ps =
  (* NOTE(dbp 2015-03-13): We want just:
     A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ ).
            As per the docs:
            http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
            Uri has that as it's fall-through, which at least currently (and hopefully forever)
            ~component:`Authority causes it to use.
  *)
  let encoded =
    List.map
      (fun (k, v) ->
        let key = Uri.pct_encode ~component:`Authority k in
        let value =
          match v with
          | [] -> ""
          | [ x ] -> Uri.pct_encode ~component:`Authority x
          | _ -> failwith "AWS query cannot have multiple values for same key"
        in
        key, value)
      ps
  in
  let sorted = List.sort (fun a b -> compare (fst a) (fst b)) encoded in
  let joined = List.map (fun (k, v) -> k ^ "=" ^ v) sorted in
  String.concat "&" joined

(* NOTE(dbp 2015-01-13): This is a direct translation of reference implementation at:
 * http://docs.aws.amazon.com/general/latest/gr/sigv4-signed-request-examples.html
 *)
let sign_request ~access_key ~secret_key ?token ~service ~region (meth, uri, headers) =
  let host = Util.of_option_exn (Endpoints.endpoint_of service region) in
  let params = encode_query (Uri.query uri) in
  let sign key msg = Hash.sha256 ~key msg in
  let get_signature_key key date region service =
    sign (sign (sign (sign ("AWS4" ^ key) date) region) service) "aws4_request"
  in
  let now = Time.now_utc () in
  let amzdate = Time.date_time now in
  let datestamp = Time.date_yymmdd now in
  let canonical_uri = "/" in
  let canonical_querystring = params in
  let payload_hash = Hash.sha256_hex "" in
  let token_header, sig_header =
    match token with
    | Some t ->
        let th = "x-amz-security-token:" ^ t ^ "\n" in
        let sh = ";x-amz-security-token" in
        th, sh
    | None -> "", ""
  in
  let canonical_headers =
    "host:"
    ^ host
    ^ "\n"
    ^ "x-amz-content-sha256:"
    ^ payload_hash
    ^ "\nx-amz-date:"
    ^ amzdate
    ^ "\n"
    ^ token_header
  in
  let signed_headers = "host;x-amz-content-sha256;x-amz-date" ^ sig_header in
  let canonical_request =
    Request.string_of_meth meth
    ^ "\n"
    ^ canonical_uri
    ^ "\n"
    ^ canonical_querystring
    ^ "\n"
    ^ canonical_headers
    ^ "\n"
    ^ signed_headers
    ^ "\n"
    ^ payload_hash
  in
  let algorithm = "AWS4-HMAC-SHA256" in
  let credential_scope =
    datestamp ^ "/" ^ region ^ "/" ^ service ^ "/" ^ "aws4_request"
  in
  let string_to_sign =
    algorithm
    ^ "\n"
    ^ amzdate
    ^ "\n"
    ^ credential_scope
    ^ "\n"
    ^ Hash.sha256_hex canonical_request
  in
  let signing_key = get_signature_key secret_key datestamp region service in
  let signature = Hash.sha256_hex ~key:signing_key string_to_sign in
  let authorization_header =
    String.concat
      ""
      [ algorithm
      ; " "
      ; "Credential="
      ; access_key
      ; "/"
      ; credential_scope
      ; ", "
      ; "SignedHeaders="
      ; signed_headers
      ; ", "
      ; "Signature="
      ; signature
      ]
  in
  let headers =
    ("x-amz-date", amzdate)
    :: ("x-amz-content-sha256", payload_hash)
    :: ("Authorization", authorization_header)
    :: headers
  in
  let full_headers =
    match token with
    | Some t -> ("X-Amz-Security-Token", t) :: headers
    | None -> headers
  in
  meth, uri, full_headers

let sign_v2_request ~access_key ~secret_key ?token ~service ~region (meth, uri, headers) =
  let host = Util.of_option_exn (Endpoints.endpoint_of service region) in
  let amzdate = Time.date_time_iso8601 (Time.now_utc ()) in

  let query =
    Uri.add_query_params'
      uri
      ((match token with
       | Some t -> [ "SecurityToken", t ]
       | None -> [])
      @ [ "Timestamp", amzdate
        ; "AWSAccessKeyId", access_key
        ; "SignatureMethod", "HmacSHA256"
        ; "SignatureVersion", "2"
        ])
  in

  let params = encode_query (Uri.query query) in
  let canonical_uri = "/" in
  let string_to_sign =
    Request.string_of_meth meth ^ "\n" ^ host ^ "\n" ^ canonical_uri ^ "\n" ^ params
  in
  let signature = Base64.encode_string @@ Hash.sha256 ~key:secret_key string_to_sign in
  let new_uri = Uri.add_query_param' query ("Signature", signature) in
  meth, new_uri, headers
