open Aws.BaseTypes

type t =
  { name : String.t option
  ; kms_arn : String.t option
  ; flag : Integer.t option
  ; signing_algorithm_mnemonic : String.t option
  ; signing_algorithm_type : Integer.t option
  ; digest_algorithm_mnemonic : String.t option
  ; digest_algorithm_type : Integer.t option
  ; key_tag : Integer.t option
  ; digest_value : String.t option
  ; public_key : String.t option
  ; d_s_record : String.t option
  ; d_n_s_k_e_y_record : String.t option
  ; status : String.t option
  ; status_message : String.t option
  ; created_date : DateTime.t option
  ; last_modified_date : DateTime.t option
  }

let make
    ?name
    ?kms_arn
    ?flag
    ?signing_algorithm_mnemonic
    ?signing_algorithm_type
    ?digest_algorithm_mnemonic
    ?digest_algorithm_type
    ?key_tag
    ?digest_value
    ?public_key
    ?d_s_record
    ?d_n_s_k_e_y_record
    ?status
    ?status_message
    ?created_date
    ?last_modified_date
    () =
  { name
  ; kms_arn
  ; flag
  ; signing_algorithm_mnemonic
  ; signing_algorithm_type
  ; digest_algorithm_mnemonic
  ; digest_algorithm_type
  ; key_tag
  ; digest_value
  ; public_key
  ; d_s_record
  ; d_n_s_k_e_y_record
  ; status
  ; status_message
  ; created_date
  ; last_modified_date
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; kms_arn = Aws.Util.option_bind (Aws.Xml.member "KmsArn" xml) String.parse
    ; flag = Aws.Util.option_bind (Aws.Xml.member "Flag" xml) Integer.parse
    ; signing_algorithm_mnemonic =
        Aws.Util.option_bind (Aws.Xml.member "SigningAlgorithmMnemonic" xml) String.parse
    ; signing_algorithm_type =
        Aws.Util.option_bind (Aws.Xml.member "SigningAlgorithmType" xml) Integer.parse
    ; digest_algorithm_mnemonic =
        Aws.Util.option_bind (Aws.Xml.member "DigestAlgorithmMnemonic" xml) String.parse
    ; digest_algorithm_type =
        Aws.Util.option_bind (Aws.Xml.member "DigestAlgorithmType" xml) Integer.parse
    ; key_tag = Aws.Util.option_bind (Aws.Xml.member "KeyTag" xml) Integer.parse
    ; digest_value = Aws.Util.option_bind (Aws.Xml.member "DigestValue" xml) String.parse
    ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) String.parse
    ; d_s_record = Aws.Util.option_bind (Aws.Xml.member "DSRecord" xml) String.parse
    ; d_n_s_k_e_y_record =
        Aws.Util.option_bind (Aws.Xml.member "DNSKEYRecord" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
    ; created_date =
        Aws.Util.option_bind (Aws.Xml.member "CreatedDate" xml) DateTime.parse
    ; last_modified_date =
        Aws.Util.option_bind (Aws.Xml.member "LastModifiedDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_modified_date (fun f ->
             Aws.Query.Pair ("LastModifiedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.created_date (fun f ->
             Aws.Query.Pair ("CreatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.d_n_s_k_e_y_record (fun f ->
             Aws.Query.Pair ("DNSKEYRecord", String.to_query f))
       ; Aws.Util.option_map v.d_s_record (fun f ->
             Aws.Query.Pair ("DSRecord", String.to_query f))
       ; Aws.Util.option_map v.public_key (fun f ->
             Aws.Query.Pair ("PublicKey", String.to_query f))
       ; Aws.Util.option_map v.digest_value (fun f ->
             Aws.Query.Pair ("DigestValue", String.to_query f))
       ; Aws.Util.option_map v.key_tag (fun f ->
             Aws.Query.Pair ("KeyTag", Integer.to_query f))
       ; Aws.Util.option_map v.digest_algorithm_type (fun f ->
             Aws.Query.Pair ("DigestAlgorithmType", Integer.to_query f))
       ; Aws.Util.option_map v.digest_algorithm_mnemonic (fun f ->
             Aws.Query.Pair ("DigestAlgorithmMnemonic", String.to_query f))
       ; Aws.Util.option_map v.signing_algorithm_type (fun f ->
             Aws.Query.Pair ("SigningAlgorithmType", Integer.to_query f))
       ; Aws.Util.option_map v.signing_algorithm_mnemonic (fun f ->
             Aws.Query.Pair ("SigningAlgorithmMnemonic", String.to_query f))
       ; Aws.Util.option_map v.flag (fun f -> Aws.Query.Pair ("Flag", Integer.to_query f))
       ; Aws.Util.option_map v.kms_arn (fun f ->
             Aws.Query.Pair ("KmsArn", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_modified_date (fun f ->
             "LastModifiedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.created_date (fun f -> "CreatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "StatusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.d_n_s_k_e_y_record (fun f ->
             "DNSKEYRecord", String.to_json f)
       ; Aws.Util.option_map v.d_s_record (fun f -> "DSRecord", String.to_json f)
       ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", String.to_json f)
       ; Aws.Util.option_map v.digest_value (fun f -> "DigestValue", String.to_json f)
       ; Aws.Util.option_map v.key_tag (fun f -> "KeyTag", Integer.to_json f)
       ; Aws.Util.option_map v.digest_algorithm_type (fun f ->
             "DigestAlgorithmType", Integer.to_json f)
       ; Aws.Util.option_map v.digest_algorithm_mnemonic (fun f ->
             "DigestAlgorithmMnemonic", String.to_json f)
       ; Aws.Util.option_map v.signing_algorithm_type (fun f ->
             "SigningAlgorithmType", Integer.to_json f)
       ; Aws.Util.option_map v.signing_algorithm_mnemonic (fun f ->
             "SigningAlgorithmMnemonic", String.to_json f)
       ; Aws.Util.option_map v.flag (fun f -> "Flag", Integer.to_json f)
       ; Aws.Util.option_map v.kms_arn (fun f -> "KmsArn", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; kms_arn = Aws.Util.option_map (Aws.Json.lookup j "KmsArn") String.of_json
  ; flag = Aws.Util.option_map (Aws.Json.lookup j "Flag") Integer.of_json
  ; signing_algorithm_mnemonic =
      Aws.Util.option_map (Aws.Json.lookup j "SigningAlgorithmMnemonic") String.of_json
  ; signing_algorithm_type =
      Aws.Util.option_map (Aws.Json.lookup j "SigningAlgorithmType") Integer.of_json
  ; digest_algorithm_mnemonic =
      Aws.Util.option_map (Aws.Json.lookup j "DigestAlgorithmMnemonic") String.of_json
  ; digest_algorithm_type =
      Aws.Util.option_map (Aws.Json.lookup j "DigestAlgorithmType") Integer.of_json
  ; key_tag = Aws.Util.option_map (Aws.Json.lookup j "KeyTag") Integer.of_json
  ; digest_value = Aws.Util.option_map (Aws.Json.lookup j "DigestValue") String.of_json
  ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") String.of_json
  ; d_s_record = Aws.Util.option_map (Aws.Json.lookup j "DSRecord") String.of_json
  ; d_n_s_k_e_y_record =
      Aws.Util.option_map (Aws.Json.lookup j "DNSKEYRecord") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
  ; created_date = Aws.Util.option_map (Aws.Json.lookup j "CreatedDate") DateTime.of_json
  ; last_modified_date =
      Aws.Util.option_map (Aws.Json.lookup j "LastModifiedDate") DateTime.of_json
  }
