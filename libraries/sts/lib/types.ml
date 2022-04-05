open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module PolicyDescriptorType =
  struct
    type t = {
      arn: String.t option }
    let make ?arn  () = { arn }
    let parse xml =
      Some { arn = (Util.option_bind (Xml.member "arn" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.arn
              (fun f -> Query.Pair ("arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      { arn = (Util.option_map (Json.lookup j "arn") String.of_json) }
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module Credentials =
  struct
    type t =
      {
      access_key_id: String.t ;
      secret_access_key: String.t ;
      session_token: String.t ;
      expiration: DateTime.t }
    let make ~access_key_id  ~secret_access_key  ~session_token  ~expiration 
      () = { access_key_id; secret_access_key; session_token; expiration }
    let parse xml =
      Some
        {
          access_key_id =
            (Xml.required "AccessKeyId"
               (Util.option_bind (Xml.member "AccessKeyId" xml) String.parse));
          secret_access_key =
            (Xml.required "SecretAccessKey"
               (Util.option_bind (Xml.member "SecretAccessKey" xml)
                  String.parse));
          session_token =
            (Xml.required "SessionToken"
               (Util.option_bind (Xml.member "SessionToken" xml) String.parse));
          expiration =
            (Xml.required "Expiration"
               (Util.option_bind (Xml.member "Expiration" xml) DateTime.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Expiration", (DateTime.to_query v.expiration)));
           Some
             (Query.Pair ("SessionToken", (String.to_query v.session_token)));
           Some
             (Query.Pair
                ("SecretAccessKey", (String.to_query v.secret_access_key)));
           Some
             (Query.Pair ("AccessKeyId", (String.to_query v.access_key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("expiration", (DateTime.to_json v.expiration));
           Some ("session_token", (String.to_json v.session_token));
           Some ("secret_access_key", (String.to_json v.secret_access_key));
           Some ("access_key_id", (String.to_json v.access_key_id))])
    let of_json j =
      {
        access_key_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "access_key_id")));
        secret_access_key =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "secret_access_key")));
        session_token =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "session_token")));
        expiration =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "expiration")))
      }
  end
module FederatedUser =
  struct
    type t = {
      federated_user_id: String.t ;
      arn: String.t }
    let make ~federated_user_id  ~arn  () = { federated_user_id; arn }
    let parse xml =
      Some
        {
          federated_user_id =
            (Xml.required "FederatedUserId"
               (Util.option_bind (Xml.member "FederatedUserId" xml)
                  String.parse));
          arn =
            (Xml.required "Arn"
               (Util.option_bind (Xml.member "Arn" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Arn", (String.to_query v.arn)));
           Some
             (Query.Pair
                ("FederatedUserId", (String.to_query v.federated_user_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("arn", (String.to_json v.arn));
           Some ("federated_user_id", (String.to_json v.federated_user_id))])
    let of_json j =
      {
        federated_user_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "federated_user_id")));
        arn = (String.of_json (Util.of_option_exn (Json.lookup j "arn")))
      }
  end
module PolicyDescriptorListType =
  struct
    type t = PolicyDescriptorType.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyDescriptorType.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PolicyDescriptorType.to_query v
    let to_json v = `List (List.map PolicyDescriptorType.to_json v)
    let of_json j = Json.to_list PolicyDescriptorType.of_json j
  end
module TagListType =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module AssumedRoleUser =
  struct
    type t = {
      assumed_role_id: String.t ;
      arn: String.t }
    let make ~assumed_role_id  ~arn  () = { assumed_role_id; arn }
    let parse xml =
      Some
        {
          assumed_role_id =
            (Xml.required "AssumedRoleId"
               (Util.option_bind (Xml.member "AssumedRoleId" xml)
                  String.parse));
          arn =
            (Xml.required "Arn"
               (Util.option_bind (Xml.member "Arn" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Arn", (String.to_query v.arn)));
           Some
             (Query.Pair
                ("AssumedRoleId", (String.to_query v.assumed_role_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("arn", (String.to_json v.arn));
           Some ("assumed_role_id", (String.to_json v.assumed_role_id))])
    let of_json j =
      {
        assumed_role_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "assumed_role_id")));
        arn = (String.of_json (Util.of_option_exn (Json.lookup j "arn")))
      }
  end
module TagKeyListType =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module PackedPolicyTooLargeException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetFederationTokenResponse =
  struct
    type t =
      {
      credentials: Credentials.t option ;
      federated_user: FederatedUser.t option ;
      packed_policy_size: Integer.t option }
    let make ?credentials  ?federated_user  ?packed_policy_size  () =
      { credentials; federated_user; packed_policy_size }
    let parse xml =
      Some
        {
          credentials =
            (Util.option_bind (Xml.member "Credentials" xml)
               Credentials.parse);
          federated_user =
            (Util.option_bind (Xml.member "FederatedUser" xml)
               FederatedUser.parse);
          packed_policy_size =
            (Util.option_bind (Xml.member "PackedPolicySize" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.packed_policy_size
              (fun f -> Query.Pair ("PackedPolicySize", (Integer.to_query f)));
           Util.option_map v.federated_user
             (fun f ->
                Query.Pair ("FederatedUser", (FederatedUser.to_query f)));
           Util.option_map v.credentials
             (fun f -> Query.Pair ("Credentials", (Credentials.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.packed_policy_size
              (fun f -> ("packed_policy_size", (Integer.to_json f)));
           Util.option_map v.federated_user
             (fun f -> ("federated_user", (FederatedUser.to_json f)));
           Util.option_map v.credentials
             (fun f -> ("credentials", (Credentials.to_json f)))])
    let of_json j =
      {
        credentials =
          (Util.option_map (Json.lookup j "credentials") Credentials.of_json);
        federated_user =
          (Util.option_map (Json.lookup j "federated_user")
             FederatedUser.of_json);
        packed_policy_size =
          (Util.option_map (Json.lookup j "packed_policy_size")
             Integer.of_json)
      }
  end
module InvalidIdentityTokenException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetAccessKeyInfoRequest =
  struct
    type t = {
      access_key_id: String.t }
    let make ~access_key_id  () = { access_key_id }
    let parse xml =
      Some
        {
          access_key_id =
            (Xml.required "AccessKeyId"
               (Util.option_bind (Xml.member "AccessKeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("AccessKeyId", (String.to_query v.access_key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("access_key_id", (String.to_json v.access_key_id))])
    let of_json j =
      {
        access_key_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "access_key_id")))
      }
  end
module GetFederationTokenRequest =
  struct
    type t =
      {
      name: String.t ;
      policy: String.t option ;
      policy_arns: PolicyDescriptorListType.t ;
      duration_seconds: Integer.t option ;
      tags: TagListType.t }
    let make ~name  ?policy  ?(policy_arns= [])  ?duration_seconds  ?(tags=
      [])  () = { name; policy; policy_arns; duration_seconds; tags }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          policy = (Util.option_bind (Xml.member "Policy" xml) String.parse);
          policy_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyArns" xml)
                  PolicyDescriptorListType.parse));
          duration_seconds =
            (Util.option_bind (Xml.member "DurationSeconds" xml)
               Integer.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagListType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagListType.to_query v.tags)));
           Util.option_map v.duration_seconds
             (fun f -> Query.Pair ("DurationSeconds", (Integer.to_query f)));
           Some
             (Query.Pair
                ("PolicyArns.member",
                  (PolicyDescriptorListType.to_query v.policy_arns)));
           Util.option_map v.policy
             (fun f -> Query.Pair ("Policy", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagListType.to_json v.tags));
           Util.option_map v.duration_seconds
             (fun f -> ("duration_seconds", (Integer.to_json f)));
           Some
             ("policy_arns",
               (PolicyDescriptorListType.to_json v.policy_arns));
           Util.option_map v.policy (fun f -> ("policy", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        policy = (Util.option_map (Json.lookup j "policy") String.of_json);
        policy_arns =
          (PolicyDescriptorListType.of_json
             (Util.of_option_exn (Json.lookup j "policy_arns")));
        duration_seconds =
          (Util.option_map (Json.lookup j "duration_seconds") Integer.of_json);
        tags =
          (TagListType.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module IDPRejectedClaimException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module AssumeRoleWithSAMLResponse =
  struct
    type t =
      {
      credentials: Credentials.t option ;
      assumed_role_user: AssumedRoleUser.t option ;
      packed_policy_size: Integer.t option ;
      subject: String.t option ;
      subject_type: String.t option ;
      issuer: String.t option ;
      audience: String.t option ;
      name_qualifier: String.t option }
    let make ?credentials  ?assumed_role_user  ?packed_policy_size  ?subject 
      ?subject_type  ?issuer  ?audience  ?name_qualifier  () =
      {
        credentials;
        assumed_role_user;
        packed_policy_size;
        subject;
        subject_type;
        issuer;
        audience;
        name_qualifier
      }
    let parse xml =
      Some
        {
          credentials =
            (Util.option_bind (Xml.member "Credentials" xml)
               Credentials.parse);
          assumed_role_user =
            (Util.option_bind (Xml.member "AssumedRoleUser" xml)
               AssumedRoleUser.parse);
          packed_policy_size =
            (Util.option_bind (Xml.member "PackedPolicySize" xml)
               Integer.parse);
          subject =
            (Util.option_bind (Xml.member "Subject" xml) String.parse);
          subject_type =
            (Util.option_bind (Xml.member "SubjectType" xml) String.parse);
          issuer = (Util.option_bind (Xml.member "Issuer" xml) String.parse);
          audience =
            (Util.option_bind (Xml.member "Audience" xml) String.parse);
          name_qualifier =
            (Util.option_bind (Xml.member "NameQualifier" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name_qualifier
              (fun f -> Query.Pair ("NameQualifier", (String.to_query f)));
           Util.option_map v.audience
             (fun f -> Query.Pair ("Audience", (String.to_query f)));
           Util.option_map v.issuer
             (fun f -> Query.Pair ("Issuer", (String.to_query f)));
           Util.option_map v.subject_type
             (fun f -> Query.Pair ("SubjectType", (String.to_query f)));
           Util.option_map v.subject
             (fun f -> Query.Pair ("Subject", (String.to_query f)));
           Util.option_map v.packed_policy_size
             (fun f -> Query.Pair ("PackedPolicySize", (Integer.to_query f)));
           Util.option_map v.assumed_role_user
             (fun f ->
                Query.Pair ("AssumedRoleUser", (AssumedRoleUser.to_query f)));
           Util.option_map v.credentials
             (fun f -> Query.Pair ("Credentials", (Credentials.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name_qualifier
              (fun f -> ("name_qualifier", (String.to_json f)));
           Util.option_map v.audience
             (fun f -> ("audience", (String.to_json f)));
           Util.option_map v.issuer (fun f -> ("issuer", (String.to_json f)));
           Util.option_map v.subject_type
             (fun f -> ("subject_type", (String.to_json f)));
           Util.option_map v.subject
             (fun f -> ("subject", (String.to_json f)));
           Util.option_map v.packed_policy_size
             (fun f -> ("packed_policy_size", (Integer.to_json f)));
           Util.option_map v.assumed_role_user
             (fun f -> ("assumed_role_user", (AssumedRoleUser.to_json f)));
           Util.option_map v.credentials
             (fun f -> ("credentials", (Credentials.to_json f)))])
    let of_json j =
      {
        credentials =
          (Util.option_map (Json.lookup j "credentials") Credentials.of_json);
        assumed_role_user =
          (Util.option_map (Json.lookup j "assumed_role_user")
             AssumedRoleUser.of_json);
        packed_policy_size =
          (Util.option_map (Json.lookup j "packed_policy_size")
             Integer.of_json);
        subject = (Util.option_map (Json.lookup j "subject") String.of_json);
        subject_type =
          (Util.option_map (Json.lookup j "subject_type") String.of_json);
        issuer = (Util.option_map (Json.lookup j "issuer") String.of_json);
        audience =
          (Util.option_map (Json.lookup j "audience") String.of_json);
        name_qualifier =
          (Util.option_map (Json.lookup j "name_qualifier") String.of_json)
      }
  end
module MalformedPolicyDocumentException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module RegionDisabledException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module IDPCommunicationErrorException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetCallerIdentityRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ExpiredTokenException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DecodeAuthorizationMessageResponse =
  struct
    type t = {
      decoded_message: String.t option }
    let make ?decoded_message  () = { decoded_message }
    let parse xml =
      Some
        {
          decoded_message =
            (Util.option_bind (Xml.member "DecodedMessage" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.decoded_message
              (fun f -> Query.Pair ("DecodedMessage", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.decoded_message
              (fun f -> ("decoded_message", (String.to_json f)))])
    let of_json j =
      {
        decoded_message =
          (Util.option_map (Json.lookup j "decoded_message") String.of_json)
      }
  end
module AssumeRoleResponse =
  struct
    type t =
      {
      credentials: Credentials.t option ;
      assumed_role_user: AssumedRoleUser.t option ;
      packed_policy_size: Integer.t option }
    let make ?credentials  ?assumed_role_user  ?packed_policy_size  () =
      { credentials; assumed_role_user; packed_policy_size }
    let parse xml =
      Some
        {
          credentials =
            (Util.option_bind (Xml.member "Credentials" xml)
               Credentials.parse);
          assumed_role_user =
            (Util.option_bind (Xml.member "AssumedRoleUser" xml)
               AssumedRoleUser.parse);
          packed_policy_size =
            (Util.option_bind (Xml.member "PackedPolicySize" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.packed_policy_size
              (fun f -> Query.Pair ("PackedPolicySize", (Integer.to_query f)));
           Util.option_map v.assumed_role_user
             (fun f ->
                Query.Pair ("AssumedRoleUser", (AssumedRoleUser.to_query f)));
           Util.option_map v.credentials
             (fun f -> Query.Pair ("Credentials", (Credentials.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.packed_policy_size
              (fun f -> ("packed_policy_size", (Integer.to_json f)));
           Util.option_map v.assumed_role_user
             (fun f -> ("assumed_role_user", (AssumedRoleUser.to_json f)));
           Util.option_map v.credentials
             (fun f -> ("credentials", (Credentials.to_json f)))])
    let of_json j =
      {
        credentials =
          (Util.option_map (Json.lookup j "credentials") Credentials.of_json);
        assumed_role_user =
          (Util.option_map (Json.lookup j "assumed_role_user")
             AssumedRoleUser.of_json);
        packed_policy_size =
          (Util.option_map (Json.lookup j "packed_policy_size")
             Integer.of_json)
      }
  end
module GetAccessKeyInfoResponse =
  struct
    type t = {
      account: String.t option }
    let make ?account  () = { account }
    let parse xml =
      Some
        {
          account =
            (Util.option_bind (Xml.member "Account" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.account
              (fun f -> Query.Pair ("Account", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.account
              (fun f -> ("account", (String.to_json f)))])
    let of_json j =
      { account = (Util.option_map (Json.lookup j "account") String.of_json)
      }
  end
module DecodeAuthorizationMessageRequest =
  struct
    type t = {
      encoded_message: String.t }
    let make ~encoded_message  () = { encoded_message }
    let parse xml =
      Some
        {
          encoded_message =
            (Xml.required "EncodedMessage"
               (Util.option_bind (Xml.member "EncodedMessage" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EncodedMessage", (String.to_query v.encoded_message)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("encoded_message", (String.to_json v.encoded_message))])
    let of_json j =
      {
        encoded_message =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "encoded_message")))
      }
  end
module GetSessionTokenResponse =
  struct
    type t = {
      credentials: Credentials.t option }
    let make ?credentials  () = { credentials }
    let parse xml =
      Some
        {
          credentials =
            (Util.option_bind (Xml.member "Credentials" xml)
               Credentials.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.credentials
              (fun f -> Query.Pair ("Credentials", (Credentials.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.credentials
              (fun f -> ("credentials", (Credentials.to_json f)))])
    let of_json j =
      {
        credentials =
          (Util.option_map (Json.lookup j "credentials") Credentials.of_json)
      }
  end
module InvalidAuthorizationMessageException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetSessionTokenRequest =
  struct
    type t =
      {
      duration_seconds: Integer.t option ;
      serial_number: String.t option ;
      token_code: String.t option }
    let make ?duration_seconds  ?serial_number  ?token_code  () =
      { duration_seconds; serial_number; token_code }
    let parse xml =
      Some
        {
          duration_seconds =
            (Util.option_bind (Xml.member "DurationSeconds" xml)
               Integer.parse);
          serial_number =
            (Util.option_bind (Xml.member "SerialNumber" xml) String.parse);
          token_code =
            (Util.option_bind (Xml.member "TokenCode" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.token_code
              (fun f -> Query.Pair ("TokenCode", (String.to_query f)));
           Util.option_map v.serial_number
             (fun f -> Query.Pair ("SerialNumber", (String.to_query f)));
           Util.option_map v.duration_seconds
             (fun f -> Query.Pair ("DurationSeconds", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.token_code
              (fun f -> ("token_code", (String.to_json f)));
           Util.option_map v.serial_number
             (fun f -> ("serial_number", (String.to_json f)));
           Util.option_map v.duration_seconds
             (fun f -> ("duration_seconds", (Integer.to_json f)))])
    let of_json j =
      {
        duration_seconds =
          (Util.option_map (Json.lookup j "duration_seconds") Integer.of_json);
        serial_number =
          (Util.option_map (Json.lookup j "serial_number") String.of_json);
        token_code =
          (Util.option_map (Json.lookup j "token_code") String.of_json)
      }
  end
module AssumeRoleWithSAMLRequest =
  struct
    type t =
      {
      role_arn: String.t ;
      principal_arn: String.t ;
      s_a_m_l_assertion: String.t ;
      policy_arns: PolicyDescriptorListType.t ;
      policy: String.t option ;
      duration_seconds: Integer.t option }
    let make ~role_arn  ~principal_arn  ~s_a_m_l_assertion  ?(policy_arns=
      [])  ?policy  ?duration_seconds  () =
      {
        role_arn;
        principal_arn;
        s_a_m_l_assertion;
        policy_arns;
        policy;
        duration_seconds
      }
    let parse xml =
      Some
        {
          role_arn =
            (Xml.required "RoleArn"
               (Util.option_bind (Xml.member "RoleArn" xml) String.parse));
          principal_arn =
            (Xml.required "PrincipalArn"
               (Util.option_bind (Xml.member "PrincipalArn" xml) String.parse));
          s_a_m_l_assertion =
            (Xml.required "SAMLAssertion"
               (Util.option_bind (Xml.member "SAMLAssertion" xml)
                  String.parse));
          policy_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyArns" xml)
                  PolicyDescriptorListType.parse));
          policy = (Util.option_bind (Xml.member "Policy" xml) String.parse);
          duration_seconds =
            (Util.option_bind (Xml.member "DurationSeconds" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.duration_seconds
              (fun f -> Query.Pair ("DurationSeconds", (Integer.to_query f)));
           Util.option_map v.policy
             (fun f -> Query.Pair ("Policy", (String.to_query f)));
           Some
             (Query.Pair
                ("PolicyArns.member",
                  (PolicyDescriptorListType.to_query v.policy_arns)));
           Some
             (Query.Pair
                ("SAMLAssertion", (String.to_query v.s_a_m_l_assertion)));
           Some
             (Query.Pair ("PrincipalArn", (String.to_query v.principal_arn)));
           Some (Query.Pair ("RoleArn", (String.to_query v.role_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.duration_seconds
              (fun f -> ("duration_seconds", (Integer.to_json f)));
           Util.option_map v.policy (fun f -> ("policy", (String.to_json f)));
           Some
             ("policy_arns",
               (PolicyDescriptorListType.to_json v.policy_arns));
           Some ("s_a_m_l_assertion", (String.to_json v.s_a_m_l_assertion));
           Some ("principal_arn", (String.to_json v.principal_arn));
           Some ("role_arn", (String.to_json v.role_arn))])
    let of_json j =
      {
        role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "role_arn")));
        principal_arn =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "principal_arn")));
        s_a_m_l_assertion =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s_a_m_l_assertion")));
        policy_arns =
          (PolicyDescriptorListType.of_json
             (Util.of_option_exn (Json.lookup j "policy_arns")));
        policy = (Util.option_map (Json.lookup j "policy") String.of_json);
        duration_seconds =
          (Util.option_map (Json.lookup j "duration_seconds") Integer.of_json)
      }
  end
module AssumeRoleWithWebIdentityResponse =
  struct
    type t =
      {
      credentials: Credentials.t option ;
      subject_from_web_identity_token: String.t option ;
      assumed_role_user: AssumedRoleUser.t option ;
      packed_policy_size: Integer.t option ;
      provider: String.t option ;
      audience: String.t option }
    let make ?credentials  ?subject_from_web_identity_token 
      ?assumed_role_user  ?packed_policy_size  ?provider  ?audience  () =
      {
        credentials;
        subject_from_web_identity_token;
        assumed_role_user;
        packed_policy_size;
        provider;
        audience
      }
    let parse xml =
      Some
        {
          credentials =
            (Util.option_bind (Xml.member "Credentials" xml)
               Credentials.parse);
          subject_from_web_identity_token =
            (Util.option_bind (Xml.member "SubjectFromWebIdentityToken" xml)
               String.parse);
          assumed_role_user =
            (Util.option_bind (Xml.member "AssumedRoleUser" xml)
               AssumedRoleUser.parse);
          packed_policy_size =
            (Util.option_bind (Xml.member "PackedPolicySize" xml)
               Integer.parse);
          provider =
            (Util.option_bind (Xml.member "Provider" xml) String.parse);
          audience =
            (Util.option_bind (Xml.member "Audience" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.audience
              (fun f -> Query.Pair ("Audience", (String.to_query f)));
           Util.option_map v.provider
             (fun f -> Query.Pair ("Provider", (String.to_query f)));
           Util.option_map v.packed_policy_size
             (fun f -> Query.Pair ("PackedPolicySize", (Integer.to_query f)));
           Util.option_map v.assumed_role_user
             (fun f ->
                Query.Pair ("AssumedRoleUser", (AssumedRoleUser.to_query f)));
           Util.option_map v.subject_from_web_identity_token
             (fun f ->
                Query.Pair
                  ("SubjectFromWebIdentityToken", (String.to_query f)));
           Util.option_map v.credentials
             (fun f -> Query.Pair ("Credentials", (Credentials.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.audience
              (fun f -> ("audience", (String.to_json f)));
           Util.option_map v.provider
             (fun f -> ("provider", (String.to_json f)));
           Util.option_map v.packed_policy_size
             (fun f -> ("packed_policy_size", (Integer.to_json f)));
           Util.option_map v.assumed_role_user
             (fun f -> ("assumed_role_user", (AssumedRoleUser.to_json f)));
           Util.option_map v.subject_from_web_identity_token
             (fun f ->
                ("subject_from_web_identity_token", (String.to_json f)));
           Util.option_map v.credentials
             (fun f -> ("credentials", (Credentials.to_json f)))])
    let of_json j =
      {
        credentials =
          (Util.option_map (Json.lookup j "credentials") Credentials.of_json);
        subject_from_web_identity_token =
          (Util.option_map (Json.lookup j "subject_from_web_identity_token")
             String.of_json);
        assumed_role_user =
          (Util.option_map (Json.lookup j "assumed_role_user")
             AssumedRoleUser.of_json);
        packed_policy_size =
          (Util.option_map (Json.lookup j "packed_policy_size")
             Integer.of_json);
        provider =
          (Util.option_map (Json.lookup j "provider") String.of_json);
        audience =
          (Util.option_map (Json.lookup j "audience") String.of_json)
      }
  end
module GetCallerIdentityResponse =
  struct
    type t =
      {
      user_id: String.t option ;
      account: String.t option ;
      arn: String.t option }
    let make ?user_id  ?account  ?arn  () = { user_id; account; arn }
    let parse xml =
      Some
        {
          user_id = (Util.option_bind (Xml.member "UserId" xml) String.parse);
          account =
            (Util.option_bind (Xml.member "Account" xml) String.parse);
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.arn
              (fun f -> Query.Pair ("Arn", (String.to_query f)));
           Util.option_map v.account
             (fun f -> Query.Pair ("Account", (String.to_query f)));
           Util.option_map v.user_id
             (fun f -> Query.Pair ("UserId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.arn (fun f -> ("arn", (String.to_json f)));
           Util.option_map v.account
             (fun f -> ("account", (String.to_json f)));
           Util.option_map v.user_id
             (fun f -> ("user_id", (String.to_json f)))])
    let of_json j =
      {
        user_id = (Util.option_map (Json.lookup j "user_id") String.of_json);
        account = (Util.option_map (Json.lookup j "account") String.of_json);
        arn = (Util.option_map (Json.lookup j "arn") String.of_json)
      }
  end
module AssumeRoleWithWebIdentityRequest =
  struct
    type t =
      {
      role_arn: String.t ;
      role_session_name: String.t ;
      web_identity_token: String.t ;
      provider_id: String.t option ;
      policy_arns: PolicyDescriptorListType.t ;
      policy: String.t option ;
      duration_seconds: Integer.t option }
    let make ~role_arn  ~role_session_name  ~web_identity_token  ?provider_id
       ?(policy_arns= [])  ?policy  ?duration_seconds  () =
      {
        role_arn;
        role_session_name;
        web_identity_token;
        provider_id;
        policy_arns;
        policy;
        duration_seconds
      }
    let parse xml =
      Some
        {
          role_arn =
            (Xml.required "RoleArn"
               (Util.option_bind (Xml.member "RoleArn" xml) String.parse));
          role_session_name =
            (Xml.required "RoleSessionName"
               (Util.option_bind (Xml.member "RoleSessionName" xml)
                  String.parse));
          web_identity_token =
            (Xml.required "WebIdentityToken"
               (Util.option_bind (Xml.member "WebIdentityToken" xml)
                  String.parse));
          provider_id =
            (Util.option_bind (Xml.member "ProviderId" xml) String.parse);
          policy_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyArns" xml)
                  PolicyDescriptorListType.parse));
          policy = (Util.option_bind (Xml.member "Policy" xml) String.parse);
          duration_seconds =
            (Util.option_bind (Xml.member "DurationSeconds" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.duration_seconds
              (fun f -> Query.Pair ("DurationSeconds", (Integer.to_query f)));
           Util.option_map v.policy
             (fun f -> Query.Pair ("Policy", (String.to_query f)));
           Some
             (Query.Pair
                ("PolicyArns.member",
                  (PolicyDescriptorListType.to_query v.policy_arns)));
           Util.option_map v.provider_id
             (fun f -> Query.Pair ("ProviderId", (String.to_query f)));
           Some
             (Query.Pair
                ("WebIdentityToken", (String.to_query v.web_identity_token)));
           Some
             (Query.Pair
                ("RoleSessionName", (String.to_query v.role_session_name)));
           Some (Query.Pair ("RoleArn", (String.to_query v.role_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.duration_seconds
              (fun f -> ("duration_seconds", (Integer.to_json f)));
           Util.option_map v.policy (fun f -> ("policy", (String.to_json f)));
           Some
             ("policy_arns",
               (PolicyDescriptorListType.to_json v.policy_arns));
           Util.option_map v.provider_id
             (fun f -> ("provider_id", (String.to_json f)));
           Some ("web_identity_token", (String.to_json v.web_identity_token));
           Some ("role_session_name", (String.to_json v.role_session_name));
           Some ("role_arn", (String.to_json v.role_arn))])
    let of_json j =
      {
        role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "role_arn")));
        role_session_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "role_session_name")));
        web_identity_token =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "web_identity_token")));
        provider_id =
          (Util.option_map (Json.lookup j "provider_id") String.of_json);
        policy_arns =
          (PolicyDescriptorListType.of_json
             (Util.of_option_exn (Json.lookup j "policy_arns")));
        policy = (Util.option_map (Json.lookup j "policy") String.of_json);
        duration_seconds =
          (Util.option_map (Json.lookup j "duration_seconds") Integer.of_json)
      }
  end
module AssumeRoleRequest =
  struct
    type t =
      {
      role_arn: String.t ;
      role_session_name: String.t ;
      policy_arns: PolicyDescriptorListType.t ;
      policy: String.t option ;
      duration_seconds: Integer.t option ;
      tags: TagListType.t ;
      transitive_tag_keys: TagKeyListType.t ;
      external_id: String.t option ;
      serial_number: String.t option ;
      token_code: String.t option }
    let make ~role_arn  ~role_session_name  ?(policy_arns= [])  ?policy 
      ?duration_seconds  ?(tags= [])  ?(transitive_tag_keys= []) 
      ?external_id  ?serial_number  ?token_code  () =
      {
        role_arn;
        role_session_name;
        policy_arns;
        policy;
        duration_seconds;
        tags;
        transitive_tag_keys;
        external_id;
        serial_number;
        token_code
      }
    let parse xml =
      Some
        {
          role_arn =
            (Xml.required "RoleArn"
               (Util.option_bind (Xml.member "RoleArn" xml) String.parse));
          role_session_name =
            (Xml.required "RoleSessionName"
               (Util.option_bind (Xml.member "RoleSessionName" xml)
                  String.parse));
          policy_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyArns" xml)
                  PolicyDescriptorListType.parse));
          policy = (Util.option_bind (Xml.member "Policy" xml) String.parse);
          duration_seconds =
            (Util.option_bind (Xml.member "DurationSeconds" xml)
               Integer.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagListType.parse));
          transitive_tag_keys =
            (Util.of_option []
               (Util.option_bind (Xml.member "TransitiveTagKeys" xml)
                  TagKeyListType.parse));
          external_id =
            (Util.option_bind (Xml.member "ExternalId" xml) String.parse);
          serial_number =
            (Util.option_bind (Xml.member "SerialNumber" xml) String.parse);
          token_code =
            (Util.option_bind (Xml.member "TokenCode" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.token_code
              (fun f -> Query.Pair ("TokenCode", (String.to_query f)));
           Util.option_map v.serial_number
             (fun f -> Query.Pair ("SerialNumber", (String.to_query f)));
           Util.option_map v.external_id
             (fun f -> Query.Pair ("ExternalId", (String.to_query f)));
           Some
             (Query.Pair
                ("TransitiveTagKeys.member",
                  (TagKeyListType.to_query v.transitive_tag_keys)));
           Some (Query.Pair ("Tags.member", (TagListType.to_query v.tags)));
           Util.option_map v.duration_seconds
             (fun f -> Query.Pair ("DurationSeconds", (Integer.to_query f)));
           Util.option_map v.policy
             (fun f -> Query.Pair ("Policy", (String.to_query f)));
           Some
             (Query.Pair
                ("PolicyArns.member",
                  (PolicyDescriptorListType.to_query v.policy_arns)));
           Some
             (Query.Pair
                ("RoleSessionName", (String.to_query v.role_session_name)));
           Some (Query.Pair ("RoleArn", (String.to_query v.role_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.token_code
              (fun f -> ("token_code", (String.to_json f)));
           Util.option_map v.serial_number
             (fun f -> ("serial_number", (String.to_json f)));
           Util.option_map v.external_id
             (fun f -> ("external_id", (String.to_json f)));
           Some
             ("transitive_tag_keys",
               (TagKeyListType.to_json v.transitive_tag_keys));
           Some ("tags", (TagListType.to_json v.tags));
           Util.option_map v.duration_seconds
             (fun f -> ("duration_seconds", (Integer.to_json f)));
           Util.option_map v.policy (fun f -> ("policy", (String.to_json f)));
           Some
             ("policy_arns",
               (PolicyDescriptorListType.to_json v.policy_arns));
           Some ("role_session_name", (String.to_json v.role_session_name));
           Some ("role_arn", (String.to_json v.role_arn))])
    let of_json j =
      {
        role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "role_arn")));
        role_session_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "role_session_name")));
        policy_arns =
          (PolicyDescriptorListType.of_json
             (Util.of_option_exn (Json.lookup j "policy_arns")));
        policy = (Util.option_map (Json.lookup j "policy") String.of_json);
        duration_seconds =
          (Util.option_map (Json.lookup j "duration_seconds") Integer.of_json);
        tags =
          (TagListType.of_json (Util.of_option_exn (Json.lookup j "tags")));
        transitive_tag_keys =
          (TagKeyListType.of_json
             (Util.of_option_exn (Json.lookup j "transitive_tag_keys")));
        external_id =
          (Util.option_map (Json.lookup j "external_id") String.of_json);
        serial_number =
          (Util.option_map (Json.lookup j "serial_number") String.of_json);
        token_code =
          (Util.option_map (Json.lookup j "token_code") String.of_json)
      }
  end