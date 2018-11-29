open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module FilterRuleName =
  struct
    type t =
      | Prefix 
      | Suffix 
    let str_to_t = [("suffix", Suffix); ("prefix", Prefix)] 
    let t_to_str = [(Suffix, "suffix"); (Prefix, "prefix")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module FilterRule =
  struct
    type t = {
      name: FilterRuleName.t option ;
      value: String.t option }
    let make ?name  ?value  () = { name; value } 
    let parse xml =
      Some
        {
          name =
            (Util.option_bind (Xml.member "Name" xml) FilterRuleName.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f  -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (FilterRuleName.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f  -> ("value", (String.to_json f)));
           Util.option_map v.name
             (fun f  -> ("name", (FilterRuleName.to_json f)))])
      
    let of_json j =
      {
        name =
          (Util.option_map (Json.lookup j "name") FilterRuleName.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      } 
  end
module Type =
  struct
    type t =
      | CanonicalUser 
      | AmazonCustomerByEmail 
      | Group 
    let str_to_t =
      [("Group", Group);
      ("AmazonCustomerByEmail", AmazonCustomerByEmail);
      ("CanonicalUser", CanonicalUser)] 
    let t_to_str =
      [(Group, "Group");
      (AmazonCustomerByEmail, "AmazonCustomerByEmail");
      (CanonicalUser, "CanonicalUser")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module FilterRuleList =
  struct
    type t = FilterRule.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map FilterRule.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list FilterRule.to_query v 
    let to_json v = `List (List.map FilterRule.to_json v) 
    let of_json j = Json.to_list FilterRule.of_json j 
  end
module TransitionStorageClass =
  struct
    type t =
      | GLACIER 
    let str_to_t = [("GLACIER", GLACIER)] 
    let t_to_str = [(GLACIER, "GLACIER")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module BucketLogsPermission =
  struct
    type t =
      | FULL_CONTROL 
      | READ 
      | WRITE 
    let str_to_t =
      [("WRITE", WRITE); ("READ", READ); ("FULL_CONTROL", FULL_CONTROL)] 
    let t_to_str =
      [(WRITE, "WRITE"); (READ, "READ"); (FULL_CONTROL, "FULL_CONTROL")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module Grantee =
  struct
    type t =
      {
      display_name: String.t option ;
      email_address: String.t option ;
      i_d: String.t option ;
      type_: Type.t ;
      u_r_i: String.t option }
    let make ?display_name  ?email_address  ?i_d  ~type_  ?u_r_i  () =
      { display_name; email_address; i_d; type_; u_r_i } 
    let parse xml =
      Some
        {
          display_name =
            (Util.option_bind (Xml.member "DisplayName" xml) String.parse);
          email_address =
            (Util.option_bind (Xml.member "EmailAddress" xml) String.parse);
          i_d = (Util.option_bind (Xml.member "ID" xml) String.parse);
          type_ =
            (Xml.required "xsi:type"
               (Util.option_bind (Xml.member "xsi:type" xml) Type.parse));
          u_r_i = (Util.option_bind (Xml.member "URI" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.u_r_i
              (fun f  -> Query.Pair ("URI", (String.to_query f)));
           Some (Query.Pair ("xsi:type", (Type.to_query v.type_)));
           Util.option_map v.i_d
             (fun f  -> Query.Pair ("ID", (String.to_query f)));
           Util.option_map v.email_address
             (fun f  -> Query.Pair ("EmailAddress", (String.to_query f)));
           Util.option_map v.display_name
             (fun f  -> Query.Pair ("DisplayName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.u_r_i (fun f  -> ("u_r_i", (String.to_json f)));
           Some ("type_", (Type.to_json v.type_));
           Util.option_map v.i_d (fun f  -> ("i_d", (String.to_json f)));
           Util.option_map v.email_address
             (fun f  -> ("email_address", (String.to_json f)));
           Util.option_map v.display_name
             (fun f  -> ("display_name", (String.to_json f)))])
      
    let of_json j =
      {
        display_name =
          (Util.option_map (Json.lookup j "display_name") String.of_json);
        email_address =
          (Util.option_map (Json.lookup j "email_address") String.of_json);
        i_d = (Util.option_map (Json.lookup j "i_d") String.of_json);
        type_ = (Type.of_json (Util.of_option_exn (Json.lookup j "type_")));
        u_r_i = (Util.option_map (Json.lookup j "u_r_i") String.of_json)
      } 
  end
module Protocol =
  struct
    type t =
      | Http 
      | Https 
    let str_to_t = [("https", Https); ("http", Http)] 
    let t_to_str = [(Https, "https"); (Http, "http")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module Event =
  struct
    type t =
      | S3_ReducedRedundancyLostObject 
      | S3_ObjectCreated__ 
      | S3_ObjectCreated_Put 
      | S3_ObjectCreated_Post 
      | S3_ObjectCreated_Copy 
      | S3_ObjectCreated_CompleteMultipartUpload 
      | S3_ObjectRemoved__ 
      | S3_ObjectRemoved_Delete 
      | S3_ObjectRemoved_DeleteMarkerCreated 
    let str_to_t =
      [("s3:ObjectRemoved:DeleteMarkerCreated",
         S3_ObjectRemoved_DeleteMarkerCreated);
      ("s3:ObjectRemoved:Delete", S3_ObjectRemoved_Delete);
      ("s3:ObjectRemoved:*", S3_ObjectRemoved__);
      ("s3:ObjectCreated:CompleteMultipartUpload",
        S3_ObjectCreated_CompleteMultipartUpload);
      ("s3:ObjectCreated:Copy", S3_ObjectCreated_Copy);
      ("s3:ObjectCreated:Post", S3_ObjectCreated_Post);
      ("s3:ObjectCreated:Put", S3_ObjectCreated_Put);
      ("s3:ObjectCreated:*", S3_ObjectCreated__);
      ("s3:ReducedRedundancyLostObject", S3_ReducedRedundancyLostObject)] 
    let t_to_str =
      [(S3_ObjectRemoved_DeleteMarkerCreated,
         "s3:ObjectRemoved:DeleteMarkerCreated");
      (S3_ObjectRemoved_Delete, "s3:ObjectRemoved:Delete");
      (S3_ObjectRemoved__, "s3:ObjectRemoved:*");
      (S3_ObjectCreated_CompleteMultipartUpload,
        "s3:ObjectCreated:CompleteMultipartUpload");
      (S3_ObjectCreated_Copy, "s3:ObjectCreated:Copy");
      (S3_ObjectCreated_Post, "s3:ObjectCreated:Post");
      (S3_ObjectCreated_Put, "s3:ObjectCreated:Put");
      (S3_ObjectCreated__, "s3:ObjectCreated:*");
      (S3_ReducedRedundancyLostObject, "s3:ReducedRedundancyLostObject")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module S3KeyFilter =
  struct
    type t = {
      filter_rules: FilterRuleList.t }
    let make ?(filter_rules= [])  () = { filter_rules } 
    let parse xml =
      Some
        {
          filter_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "FilterRule" xml)
                  FilterRuleList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("FilterRule", (FilterRuleList.to_query v.filter_rules)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("filter_rules", (FilterRuleList.to_json v.filter_rules))])
      
    let of_json j =
      {
        filter_rules =
          (FilterRuleList.of_json
             (Util.of_option_exn (Json.lookup j "filter_rules")))
      } 
  end
module Destination =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module ReplicationRuleStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)] 
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module ExpirationStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)] 
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module LifecycleExpiration =
  struct
    type t = {
      date: DateTime.t option ;
      days: Integer.t option }
    let make ?date  ?days  () = { date; days } 
    let parse xml =
      Some
        {
          date = (Util.option_bind (Xml.member "Date" xml) DateTime.parse);
          days = (Util.option_bind (Xml.member "Days" xml) Integer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.days
              (fun f  -> Query.Pair ("Days", (Integer.to_query f)));
           Util.option_map v.date
             (fun f  -> Query.Pair ("Date", (DateTime.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.days (fun f  -> ("days", (Integer.to_json f)));
           Util.option_map v.date (fun f  -> ("date", (DateTime.to_json f)))])
      
    let of_json j =
      {
        date = (Util.option_map (Json.lookup j "date") DateTime.of_json);
        days = (Util.option_map (Json.lookup j "days") Integer.of_json)
      } 
  end
module NoncurrentVersionExpiration =
  struct
    type t = {
      noncurrent_days: Integer.t }
    let make ~noncurrent_days  () = { noncurrent_days } 
    let parse xml =
      Some
        {
          noncurrent_days =
            (Xml.required "NoncurrentDays"
               (Util.option_bind (Xml.member "NoncurrentDays" xml)
                  Integer.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NoncurrentDays", (Integer.to_query v.noncurrent_days)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("noncurrent_days", (Integer.to_json v.noncurrent_days))])
      
    let of_json j =
      {
        noncurrent_days =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "noncurrent_days")))
      } 
  end
module NoncurrentVersionTransition =
  struct
    type t =
      {
      noncurrent_days: Integer.t ;
      storage_class: TransitionStorageClass.t }
    let make ~noncurrent_days  ~storage_class  () =
      { noncurrent_days; storage_class } 
    let parse xml =
      Some
        {
          noncurrent_days =
            (Xml.required "NoncurrentDays"
               (Util.option_bind (Xml.member "NoncurrentDays" xml)
                  Integer.parse));
          storage_class =
            (Xml.required "StorageClass"
               (Util.option_bind (Xml.member "StorageClass" xml)
                  TransitionStorageClass.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StorageClass",
                   (TransitionStorageClass.to_query v.storage_class)));
           Some
             (Query.Pair
                ("NoncurrentDays", (Integer.to_query v.noncurrent_days)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("storage_class",
                (TransitionStorageClass.to_json v.storage_class));
           Some ("noncurrent_days", (Integer.to_json v.noncurrent_days))])
      
    let of_json j =
      {
        noncurrent_days =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "noncurrent_days")));
        storage_class =
          (TransitionStorageClass.of_json
             (Util.of_option_exn (Json.lookup j "storage_class")))
      } 
  end
module Transition =
  struct
    type t =
      {
      date: DateTime.t option ;
      days: Integer.t option ;
      storage_class: TransitionStorageClass.t option }
    let make ?date  ?days  ?storage_class  () = { date; days; storage_class } 
    let parse xml =
      Some
        {
          date = (Util.option_bind (Xml.member "Date" xml) DateTime.parse);
          days = (Util.option_bind (Xml.member "Days" xml) Integer.parse);
          storage_class =
            (Util.option_bind (Xml.member "StorageClass" xml)
               TransitionStorageClass.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.storage_class
              (fun f  ->
                 Query.Pair
                   ("StorageClass", (TransitionStorageClass.to_query f)));
           Util.option_map v.days
             (fun f  -> Query.Pair ("Days", (Integer.to_query f)));
           Util.option_map v.date
             (fun f  -> Query.Pair ("Date", (DateTime.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.storage_class
              (fun f  ->
                 ("storage_class", (TransitionStorageClass.to_json f)));
           Util.option_map v.days (fun f  -> ("days", (Integer.to_json f)));
           Util.option_map v.date (fun f  -> ("date", (DateTime.to_json f)))])
      
    let of_json j =
      {
        date = (Util.option_map (Json.lookup j "date") DateTime.of_json);
        days = (Util.option_map (Json.lookup j "days") Integer.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             TransitionStorageClass.of_json)
      } 
  end
module Permission =
  struct
    type t =
      | FULL_CONTROL 
      | WRITE 
      | WRITE_ACP 
      | READ 
      | READ_ACP 
    let str_to_t =
      [("READ_ACP", READ_ACP);
      ("READ", READ);
      ("WRITE_ACP", WRITE_ACP);
      ("WRITE", WRITE);
      ("FULL_CONTROL", FULL_CONTROL)] 
    let t_to_str =
      [(READ_ACP, "READ_ACP");
      (READ, "READ");
      (WRITE_ACP, "WRITE_ACP");
      (WRITE, "WRITE");
      (FULL_CONTROL, "FULL_CONTROL")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module TargetGrant =
  struct
    type t =
      {
      grantee: Grantee.t option ;
      permission: BucketLogsPermission.t option }
    let make ?grantee  ?permission  () = { grantee; permission } 
    let parse xml =
      Some
        {
          grantee =
            (Util.option_bind (Xml.member "Grantee" xml) Grantee.parse);
          permission =
            (Util.option_bind (Xml.member "Permission" xml)
               BucketLogsPermission.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.permission
              (fun f  ->
                 Query.Pair ("Permission", (BucketLogsPermission.to_query f)));
           Util.option_map v.grantee
             (fun f  -> Query.Pair ("Grantee", (Grantee.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.permission
              (fun f  -> ("permission", (BucketLogsPermission.to_json f)));
           Util.option_map v.grantee
             (fun f  -> ("grantee", (Grantee.to_json f)))])
      
    let of_json j =
      {
        grantee = (Util.option_map (Json.lookup j "grantee") Grantee.of_json);
        permission =
          (Util.option_map (Json.lookup j "permission")
             BucketLogsPermission.of_json)
      } 
  end
module Condition =
  struct
    type t =
      {
      http_error_code_returned_equals: String.t option ;
      key_prefix_equals: String.t option }
    let make ?http_error_code_returned_equals  ?key_prefix_equals  () =
      { http_error_code_returned_equals; key_prefix_equals } 
    let parse xml =
      Some
        {
          http_error_code_returned_equals =
            (Util.option_bind (Xml.member "HttpErrorCodeReturnedEquals" xml)
               String.parse);
          key_prefix_equals =
            (Util.option_bind (Xml.member "KeyPrefixEquals" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_prefix_equals
              (fun f  -> Query.Pair ("KeyPrefixEquals", (String.to_query f)));
           Util.option_map v.http_error_code_returned_equals
             (fun f  ->
                Query.Pair
                  ("HttpErrorCodeReturnedEquals", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_prefix_equals
              (fun f  -> ("key_prefix_equals", (String.to_json f)));
           Util.option_map v.http_error_code_returned_equals
             (fun f  ->
                ("http_error_code_returned_equals", (String.to_json f)))])
      
    let of_json j =
      {
        http_error_code_returned_equals =
          (Util.option_map (Json.lookup j "http_error_code_returned_equals")
             String.of_json);
        key_prefix_equals =
          (Util.option_map (Json.lookup j "key_prefix_equals") String.of_json)
      } 
  end
module Redirect =
  struct
    type t =
      {
      host_name: String.t option ;
      http_redirect_code: String.t option ;
      protocol: Protocol.t option ;
      replace_key_prefix_with: String.t option ;
      replace_key_with: String.t option }
    let make ?host_name  ?http_redirect_code  ?protocol 
      ?replace_key_prefix_with  ?replace_key_with  () =
      {
        host_name;
        http_redirect_code;
        protocol;
        replace_key_prefix_with;
        replace_key_with
      } 
    let parse xml =
      Some
        {
          host_name =
            (Util.option_bind (Xml.member "HostName" xml) String.parse);
          http_redirect_code =
            (Util.option_bind (Xml.member "HttpRedirectCode" xml)
               String.parse);
          protocol =
            (Util.option_bind (Xml.member "Protocol" xml) Protocol.parse);
          replace_key_prefix_with =
            (Util.option_bind (Xml.member "ReplaceKeyPrefixWith" xml)
               String.parse);
          replace_key_with =
            (Util.option_bind (Xml.member "ReplaceKeyWith" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replace_key_with
              (fun f  -> Query.Pair ("ReplaceKeyWith", (String.to_query f)));
           Util.option_map v.replace_key_prefix_with
             (fun f  ->
                Query.Pair ("ReplaceKeyPrefixWith", (String.to_query f)));
           Util.option_map v.protocol
             (fun f  -> Query.Pair ("Protocol", (Protocol.to_query f)));
           Util.option_map v.http_redirect_code
             (fun f  -> Query.Pair ("HttpRedirectCode", (String.to_query f)));
           Util.option_map v.host_name
             (fun f  -> Query.Pair ("HostName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replace_key_with
              (fun f  -> ("replace_key_with", (String.to_json f)));
           Util.option_map v.replace_key_prefix_with
             (fun f  -> ("replace_key_prefix_with", (String.to_json f)));
           Util.option_map v.protocol
             (fun f  -> ("protocol", (Protocol.to_json f)));
           Util.option_map v.http_redirect_code
             (fun f  -> ("http_redirect_code", (String.to_json f)));
           Util.option_map v.host_name
             (fun f  -> ("host_name", (String.to_json f)))])
      
    let of_json j =
      {
        host_name =
          (Util.option_map (Json.lookup j "host_name") String.of_json);
        http_redirect_code =
          (Util.option_map (Json.lookup j "http_redirect_code")
             String.of_json);
        protocol =
          (Util.option_map (Json.lookup j "protocol") Protocol.of_json);
        replace_key_prefix_with =
          (Util.option_map (Json.lookup j "replace_key_prefix_with")
             String.of_json);
        replace_key_with =
          (Util.option_map (Json.lookup j "replace_key_with") String.of_json)
      } 
  end
module EventList =
  struct
    type t = Event.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Event.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list Event.to_query v 
    let to_json v = `List (List.map Event.to_json v) 
    let of_json j = Json.to_list Event.of_json j 
  end
module NotificationConfigurationFilter =
  struct
    type t = {
      key: S3KeyFilter.t option }
    let make ?key  () = { key } 
    let parse xml =
      Some
        { key = (Util.option_bind (Xml.member "S3Key" xml) S3KeyFilter.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key
              (fun f  -> Query.Pair ("S3Key", (S3KeyFilter.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key
              (fun f  -> ("key", (S3KeyFilter.to_json f)))])
      
    let of_json j =
      { key = (Util.option_map (Json.lookup j "key") S3KeyFilter.of_json) } 
  end
module AllowedHeaders =
  struct
    type t = String.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list String.to_query v 
    let to_json v = `List (List.map String.to_json v) 
    let of_json j = Json.to_list String.of_json j 
  end
module AllowedMethods =
  struct
    type t = String.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list String.to_query v 
    let to_json v = `List (List.map String.to_json v) 
    let of_json j = Json.to_list String.of_json j 
  end
module AllowedOrigins =
  struct
    type t = String.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list String.to_query v 
    let to_json v = `List (List.map String.to_json v) 
    let of_json j = Json.to_list String.of_json j 
  end
module ExposeHeaders =
  struct
    type t = String.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list String.to_query v 
    let to_json v = `List (List.map String.to_json v) 
    let of_json j = Json.to_list String.of_json j 
  end
module ReplicationRule =
  struct
    type t =
      {
      i_d: String.t option ;
      prefix: String.t ;
      status: ReplicationRuleStatus.t ;
      destination: Destination.t }
    let make ?i_d  ~prefix  ~status  ~destination  () =
      { i_d; prefix; status; destination } 
    let parse xml =
      Some
        {
          i_d = (Util.option_bind (Xml.member "ID" xml) String.parse);
          prefix =
            (Xml.required "Prefix"
               (Util.option_bind (Xml.member "Prefix" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml)
                  ReplicationRuleStatus.parse));
          destination =
            (Xml.required "Destination"
               (Util.option_bind (Xml.member "Destination" xml)
                  Destination.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Destination", (Destination.to_query v.destination)));
           Some
             (Query.Pair
                ("Status", (ReplicationRuleStatus.to_query v.status)));
           Some (Query.Pair ("Prefix", (String.to_query v.prefix)));
           Util.option_map v.i_d
             (fun f  -> Query.Pair ("ID", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("destination", (Destination.to_json v.destination));
           Some ("status", (ReplicationRuleStatus.to_json v.status));
           Some ("prefix", (String.to_json v.prefix));
           Util.option_map v.i_d (fun f  -> ("i_d", (String.to_json f)))])
      
    let of_json j =
      {
        i_d = (Util.option_map (Json.lookup j "i_d") String.of_json);
        prefix =
          (String.of_json (Util.of_option_exn (Json.lookup j "prefix")));
        status =
          (ReplicationRuleStatus.of_json
             (Util.of_option_exn (Json.lookup j "status")));
        destination =
          (Destination.of_json
             (Util.of_option_exn (Json.lookup j "destination")))
      } 
  end
module ObjectStorageClass =
  struct
    type t =
      | STANDARD 
      | REDUCED_REDUNDANCY 
      | GLACIER 
    let str_to_t =
      [("GLACIER", GLACIER);
      ("REDUCED_REDUNDANCY", REDUCED_REDUNDANCY);
      ("STANDARD", STANDARD)] 
    let t_to_str =
      [(GLACIER, "GLACIER");
      (REDUCED_REDUNDANCY, "REDUCED_REDUNDANCY");
      (STANDARD, "STANDARD")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module Owner =
  struct
    type t = {
      display_name: String.t option ;
      i_d: String.t option }
    let make ?display_name  ?i_d  () = { display_name; i_d } 
    let parse xml =
      Some
        {
          display_name =
            (Util.option_bind (Xml.member "DisplayName" xml) String.parse);
          i_d = (Util.option_bind (Xml.member "ID" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.i_d
              (fun f  -> Query.Pair ("ID", (String.to_query f)));
           Util.option_map v.display_name
             (fun f  -> Query.Pair ("DisplayName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.i_d (fun f  -> ("i_d", (String.to_json f)));
           Util.option_map v.display_name
             (fun f  -> ("display_name", (String.to_json f)))])
      
    let of_json j =
      {
        display_name =
          (Util.option_map (Json.lookup j "display_name") String.of_json);
        i_d = (Util.option_map (Json.lookup j "i_d") String.of_json)
      } 
  end
module Rule =
  struct
    type t =
      {
      expiration: LifecycleExpiration.t option ;
      i_d: String.t option ;
      prefix: String.t ;
      status: ExpirationStatus.t ;
      transition: Transition.t option ;
      noncurrent_version_transition: NoncurrentVersionTransition.t option ;
      noncurrent_version_expiration: NoncurrentVersionExpiration.t option }
    let make ?expiration  ?i_d  ~prefix  ~status  ?transition 
      ?noncurrent_version_transition  ?noncurrent_version_expiration  () =
      {
        expiration;
        i_d;
        prefix;
        status;
        transition;
        noncurrent_version_transition;
        noncurrent_version_expiration
      } 
    let parse xml =
      Some
        {
          expiration =
            (Util.option_bind (Xml.member "Expiration" xml)
               LifecycleExpiration.parse);
          i_d = (Util.option_bind (Xml.member "ID" xml) String.parse);
          prefix =
            (Xml.required "Prefix"
               (Util.option_bind (Xml.member "Prefix" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml)
                  ExpirationStatus.parse));
          transition =
            (Util.option_bind (Xml.member "Transition" xml) Transition.parse);
          noncurrent_version_transition =
            (Util.option_bind (Xml.member "NoncurrentVersionTransition" xml)
               NoncurrentVersionTransition.parse);
          noncurrent_version_expiration =
            (Util.option_bind (Xml.member "NoncurrentVersionExpiration" xml)
               NoncurrentVersionExpiration.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.noncurrent_version_expiration
              (fun f  ->
                 Query.Pair
                   ("NoncurrentVersionExpiration",
                     (NoncurrentVersionExpiration.to_query f)));
           Util.option_map v.noncurrent_version_transition
             (fun f  ->
                Query.Pair
                  ("NoncurrentVersionTransition",
                    (NoncurrentVersionTransition.to_query f)));
           Util.option_map v.transition
             (fun f  -> Query.Pair ("Transition", (Transition.to_query f)));
           Some (Query.Pair ("Status", (ExpirationStatus.to_query v.status)));
           Some (Query.Pair ("Prefix", (String.to_query v.prefix)));
           Util.option_map v.i_d
             (fun f  -> Query.Pair ("ID", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  ->
                Query.Pair ("Expiration", (LifecycleExpiration.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.noncurrent_version_expiration
              (fun f  ->
                 ("noncurrent_version_expiration",
                   (NoncurrentVersionExpiration.to_json f)));
           Util.option_map v.noncurrent_version_transition
             (fun f  ->
                ("noncurrent_version_transition",
                  (NoncurrentVersionTransition.to_json f)));
           Util.option_map v.transition
             (fun f  -> ("transition", (Transition.to_json f)));
           Some ("status", (ExpirationStatus.to_json v.status));
           Some ("prefix", (String.to_json v.prefix));
           Util.option_map v.i_d (fun f  -> ("i_d", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (LifecycleExpiration.to_json f)))])
      
    let of_json j =
      {
        expiration =
          (Util.option_map (Json.lookup j "expiration")
             LifecycleExpiration.of_json);
        i_d = (Util.option_map (Json.lookup j "i_d") String.of_json);
        prefix =
          (String.of_json (Util.of_option_exn (Json.lookup j "prefix")));
        status =
          (ExpirationStatus.of_json
             (Util.of_option_exn (Json.lookup j "status")));
        transition =
          (Util.option_map (Json.lookup j "transition") Transition.of_json);
        noncurrent_version_transition =
          (Util.option_map (Json.lookup j "noncurrent_version_transition")
             NoncurrentVersionTransition.of_json);
        noncurrent_version_expiration =
          (Util.option_map (Json.lookup j "noncurrent_version_expiration")
             NoncurrentVersionExpiration.of_json)
      } 
  end
module Grant =
  struct
    type t = {
      grantee: Grantee.t option ;
      permission: Permission.t option }
    let make ?grantee  ?permission  () = { grantee; permission } 
    let parse xml =
      Some
        {
          grantee =
            (Util.option_bind (Xml.member "Grantee" xml) Grantee.parse);
          permission =
            (Util.option_bind (Xml.member "Permission" xml) Permission.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.permission
              (fun f  -> Query.Pair ("Permission", (Permission.to_query f)));
           Util.option_map v.grantee
             (fun f  -> Query.Pair ("Grantee", (Grantee.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.permission
              (fun f  -> ("permission", (Permission.to_json f)));
           Util.option_map v.grantee
             (fun f  -> ("grantee", (Grantee.to_json f)))])
      
    let of_json j =
      {
        grantee = (Util.option_map (Json.lookup j "grantee") Grantee.of_json);
        permission =
          (Util.option_map (Json.lookup j "permission") Permission.of_json)
      } 
  end
module TargetGrants =
  struct
    type t = TargetGrant.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map TargetGrant.parse (Xml.members "Grant" xml)) 
    let to_query v = Query.to_query_list TargetGrant.to_query v 
    let to_json v = `List (List.map TargetGrant.to_json v) 
    let of_json j = Json.to_list TargetGrant.of_json j 
  end
module CompletedPart =
  struct
    type t = {
      e_tag: String.t ;
      part_number: Integer.t }
    let make ~e_tag  ~part_number  () = { e_tag; part_number } 
    let parse xml =
      Some
        {
          e_tag =
            (Xml.required "ETag"
               (Util.option_bind (Xml.member "ETag" xml) String.parse));
          part_number =
            (Xml.required "PartNumber"
               (Util.option_bind (Xml.member "PartNumber" xml) Integer.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("PartNumber", (Integer.to_query v.part_number)));
           Some (Query.Pair ("ETag", (String.to_query v.e_tag)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("part_number", (Integer.to_json v.part_number));
           Some ("e_tag", (String.to_json v.e_tag))])
      
    let of_json j =
      {
        e_tag = (String.of_json (Util.of_option_exn (Json.lookup j "e_tag")));
        part_number =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "part_number")))
      } 
  end
module ObjectVersionStorageClass =
  struct
    type t =
      | STANDARD 
    let str_to_t = [("STANDARD", STANDARD)] 
    let t_to_str = [(STANDARD, "STANDARD")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module ObjectIdentifier =
  struct
    type t = {
      key: String.t ;
      version_id: String.t option }
    let make ~key  ?version_id  () = { key; version_id } 
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f  -> Query.Pair ("VersionId", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f  -> ("version_id", (String.to_json f)));
           Some ("key", (String.to_json v.key))])
      
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json)
      } 
  end
module Initiator =
  struct
    type t = {
      i_d: String.t option ;
      display_name: String.t option }
    let make ?i_d  ?display_name  () = { i_d; display_name } 
    let parse xml =
      Some
        {
          i_d = (Util.option_bind (Xml.member "ID" xml) String.parse);
          display_name =
            (Util.option_bind (Xml.member "DisplayName" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.display_name
              (fun f  -> Query.Pair ("DisplayName", (String.to_query f)));
           Util.option_map v.i_d
             (fun f  -> Query.Pair ("ID", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.display_name
              (fun f  -> ("display_name", (String.to_json f)));
           Util.option_map v.i_d (fun f  -> ("i_d", (String.to_json f)))])
      
    let of_json j =
      {
        i_d = (Util.option_map (Json.lookup j "i_d") String.of_json);
        display_name =
          (Util.option_map (Json.lookup j "display_name") String.of_json)
      } 
  end
module StorageClass =
  struct
    type t =
      | STANDARD 
      | REDUCED_REDUNDANCY 
    let str_to_t =
      [("REDUCED_REDUNDANCY", REDUCED_REDUNDANCY); ("STANDARD", STANDARD)] 
    let t_to_str =
      [(REDUCED_REDUNDANCY, "REDUCED_REDUNDANCY"); (STANDARD, "STANDARD")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
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
module RoutingRule =
  struct
    type t = {
      condition: Condition.t option ;
      redirect: Redirect.t }
    let make ?condition  ~redirect  () = { condition; redirect } 
    let parse xml =
      Some
        {
          condition =
            (Util.option_bind (Xml.member "Condition" xml) Condition.parse);
          redirect =
            (Xml.required "Redirect"
               (Util.option_bind (Xml.member "Redirect" xml) Redirect.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Redirect", (Redirect.to_query v.redirect)));
           Util.option_map v.condition
             (fun f  -> Query.Pair ("Condition", (Condition.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("redirect", (Redirect.to_json v.redirect));
           Util.option_map v.condition
             (fun f  -> ("condition", (Condition.to_json f)))])
      
    let of_json j =
      {
        condition =
          (Util.option_map (Json.lookup j "condition") Condition.of_json);
        redirect =
          (Redirect.of_json (Util.of_option_exn (Json.lookup j "redirect")))
      } 
  end
module LambdaFunctionConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      lambda_function_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~lambda_function_arn  ~events  ?filter  () =
      { id; lambda_function_arn; events; filter } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          lambda_function_arn =
            (Xml.required "CloudFunction"
               (Util.option_bind (Xml.member "CloudFunction" xml)
                  String.parse));
          events =
            (Xml.required "Event"
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          filter =
            (Util.option_bind (Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Some
             (Query.Pair
                ("CloudFunction", (String.to_query v.lambda_function_arn)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 ("filter", (NotificationConfigurationFilter.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Some
             ("lambda_function_arn", (String.to_json v.lambda_function_arn));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        lambda_function_arn =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "lambda_function_arn")));
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        filter =
          (Util.option_map (Json.lookup j "filter")
             NotificationConfigurationFilter.of_json)
      } 
  end
module QueueConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      queue_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~queue_arn  ~events  ?filter  () =
      { id; queue_arn; events; filter } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          queue_arn =
            (Xml.required "Queue"
               (Util.option_bind (Xml.member "Queue" xml) String.parse));
          events =
            (Xml.required "Event"
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          filter =
            (Util.option_bind (Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Some (Query.Pair ("Queue", (String.to_query v.queue_arn)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 ("filter", (NotificationConfigurationFilter.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Some ("queue_arn", (String.to_json v.queue_arn));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        queue_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "queue_arn")));
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        filter =
          (Util.option_map (Json.lookup j "filter")
             NotificationConfigurationFilter.of_json)
      } 
  end
module TopicConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      topic_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~topic_arn  ~events  ?filter  () =
      { id; topic_arn; events; filter } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          topic_arn =
            (Xml.required "Topic"
               (Util.option_bind (Xml.member "Topic" xml) String.parse));
          events =
            (Xml.required "Event"
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          filter =
            (Util.option_bind (Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Some (Query.Pair ("Topic", (String.to_query v.topic_arn)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.filter
              (fun f  ->
                 ("filter", (NotificationConfigurationFilter.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Some ("topic_arn", (String.to_json v.topic_arn));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        topic_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "topic_arn")));
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        filter =
          (Util.option_map (Json.lookup j "filter")
             NotificationConfigurationFilter.of_json)
      } 
  end
module CORSRule =
  struct
    type t =
      {
      allowed_headers: AllowedHeaders.t ;
      allowed_methods: AllowedMethods.t ;
      allowed_origins: AllowedOrigins.t ;
      expose_headers: ExposeHeaders.t ;
      max_age_seconds: Integer.t option }
    let make ?(allowed_headers= [])  ?(allowed_methods= []) 
      ?(allowed_origins= [])  ?(expose_headers= [])  ?max_age_seconds  () =
      {
        allowed_headers;
        allowed_methods;
        allowed_origins;
        expose_headers;
        max_age_seconds
      } 
    let parse xml =
      Some
        {
          allowed_headers =
            (Util.of_option []
               (Util.option_bind (Xml.member "AllowedHeader" xml)
                  AllowedHeaders.parse));
          allowed_methods =
            (Util.of_option []
               (Util.option_bind (Xml.member "AllowedMethod" xml)
                  AllowedMethods.parse));
          allowed_origins =
            (Util.of_option []
               (Util.option_bind (Xml.member "AllowedOrigin" xml)
                  AllowedOrigins.parse));
          expose_headers =
            (Util.of_option []
               (Util.option_bind (Xml.member "ExposeHeader" xml)
                  ExposeHeaders.parse));
          max_age_seconds =
            (Util.option_bind (Xml.member "MaxAgeSeconds" xml) Integer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_age_seconds
              (fun f  -> Query.Pair ("MaxAgeSeconds", (Integer.to_query f)));
           Some
             (Query.Pair
                ("ExposeHeader", (ExposeHeaders.to_query v.expose_headers)));
           Some
             (Query.Pair
                ("AllowedOrigin",
                  (AllowedOrigins.to_query v.allowed_origins)));
           Some
             (Query.Pair
                ("AllowedMethod",
                  (AllowedMethods.to_query v.allowed_methods)));
           Some
             (Query.Pair
                ("AllowedHeader",
                  (AllowedHeaders.to_query v.allowed_headers)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_age_seconds
              (fun f  -> ("max_age_seconds", (Integer.to_json f)));
           Some ("expose_headers", (ExposeHeaders.to_json v.expose_headers));
           Some
             ("allowed_origins", (AllowedOrigins.to_json v.allowed_origins));
           Some
             ("allowed_methods", (AllowedMethods.to_json v.allowed_methods));
           Some
             ("allowed_headers", (AllowedHeaders.to_json v.allowed_headers))])
      
    let of_json j =
      {
        allowed_headers =
          (AllowedHeaders.of_json
             (Util.of_option_exn (Json.lookup j "allowed_headers")));
        allowed_methods =
          (AllowedMethods.of_json
             (Util.of_option_exn (Json.lookup j "allowed_methods")));
        allowed_origins =
          (AllowedOrigins.of_json
             (Util.of_option_exn (Json.lookup j "allowed_origins")));
        expose_headers =
          (ExposeHeaders.of_json
             (Util.of_option_exn (Json.lookup j "expose_headers")));
        max_age_seconds =
          (Util.option_map (Json.lookup j "max_age_seconds") Integer.of_json)
      } 
  end
module ReplicationRules =
  struct
    type t = ReplicationRule.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map ReplicationRule.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list ReplicationRule.to_query v 
    let to_json v = `List (List.map ReplicationRule.to_json v) 
    let of_json j = Json.to_list ReplicationRule.of_json j 
  end
module CommonPrefix =
  struct
    type t = {
      prefix: String.t option }
    let make ?prefix  () = { prefix } 
    let parse xml =
      Some
        { prefix = (Util.option_bind (Xml.member "Prefix" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.prefix
              (fun f  -> Query.Pair ("Prefix", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.prefix
              (fun f  -> ("prefix", (String.to_json f)))])
      
    let of_json j =
      { prefix = (Util.option_map (Json.lookup j "prefix") String.of_json) } 
  end
module Object =
  struct
    type t =
      {
      key: String.t ;
      last_modified: DateTime.t ;
      e_tag: String.t ;
      size: Integer.t ;
      storage_class: ObjectStorageClass.t ;
      owner: Owner.t option }
    let make ~key  ~last_modified  ~e_tag  ~size  ~storage_class  ?owner  ()
      = { key; last_modified; e_tag; size; storage_class; owner } 
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          last_modified =
            (Xml.required "LastModified"
               (Util.option_bind (Xml.member "LastModified" xml)
                  DateTime.parse));
          e_tag =
            (Xml.required "ETag"
               (Util.option_bind (Xml.member "ETag" xml) String.parse));
          size =
            (Xml.required "Size"
               (Util.option_bind (Xml.member "Size" xml) Integer.parse));
          storage_class =
            (Xml.required "StorageClass"
               (Util.option_bind (Xml.member "StorageClass" xml)
                  ObjectStorageClass.parse));
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.owner
              (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Some
             (Query.Pair
                ("StorageClass",
                  (ObjectStorageClass.to_query v.storage_class)));
           Some (Query.Pair ("Size", (Integer.to_query v.size)));
           Some (Query.Pair ("ETag", (String.to_query v.e_tag)));
           Some
             (Query.Pair
                ("LastModified", (DateTime.to_query v.last_modified)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Some
             ("storage_class", (ObjectStorageClass.to_json v.storage_class));
           Some ("size", (Integer.to_json v.size));
           Some ("e_tag", (String.to_json v.e_tag));
           Some ("last_modified", (DateTime.to_json v.last_modified));
           Some ("key", (String.to_json v.key))])
      
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        last_modified =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_modified")));
        e_tag = (String.of_json (Util.of_option_exn (Json.lookup j "e_tag")));
        size = (Integer.of_json (Util.of_option_exn (Json.lookup j "size")));
        storage_class =
          (ObjectStorageClass.of_json
             (Util.of_option_exn (Json.lookup j "storage_class")));
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json)
      } 
  end
module Rules =
  struct
    type t = Rule.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Rule.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list Rule.to_query v 
    let to_json v = `List (List.map Rule.to_json v) 
    let of_json j = Json.to_list Rule.of_json j 
  end
module Grants =
  struct
    type t = Grant.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Grant.parse (Xml.members "Grant" xml)) 
    let to_query v = Query.to_query_list Grant.to_query v 
    let to_json v = `List (List.map Grant.to_json v) 
    let of_json j = Json.to_list Grant.of_json j 
  end
module BucketLocationConstraint =
  struct
    type t =
      | EU 
      | Eu_west_1 
      | Us_west_1 
      | Us_west_2 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_northeast_1 
      | Sa_east_1 
      | Cn_north_1 
      | Eu_central_1 
    let str_to_t =
      [("eu-central-1", Eu_central_1);
      ("cn-north-1", Cn_north_1);
      ("sa-east-1", Sa_east_1);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("eu-west-1", Eu_west_1);
      ("EU", EU)] 
    let t_to_str =
      [(Eu_central_1, "eu-central-1");
      (Cn_north_1, "cn-north-1");
      (Sa_east_1, "sa-east-1");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Eu_west_1, "eu-west-1");
      (EU, "EU")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module LoggingEnabled =
  struct
    type t =
      {
      target_bucket: String.t option ;
      target_grants: TargetGrants.t ;
      target_prefix: String.t option }
    let make ?target_bucket  ?(target_grants= [])  ?target_prefix  () =
      { target_bucket; target_grants; target_prefix } 
    let parse xml =
      Some
        {
          target_bucket =
            (Util.option_bind (Xml.member "TargetBucket" xml) String.parse);
          target_grants =
            (Util.of_option []
               (Util.option_bind (Xml.member "TargetGrants" xml)
                  TargetGrants.parse));
          target_prefix =
            (Util.option_bind (Xml.member "TargetPrefix" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.target_prefix
              (fun f  -> Query.Pair ("TargetPrefix", (String.to_query f)));
           Some
             (Query.Pair
                ("TargetGrants.member",
                  (TargetGrants.to_query v.target_grants)));
           Util.option_map v.target_bucket
             (fun f  -> Query.Pair ("TargetBucket", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.target_prefix
              (fun f  -> ("target_prefix", (String.to_json f)));
           Some ("target_grants", (TargetGrants.to_json v.target_grants));
           Util.option_map v.target_bucket
             (fun f  -> ("target_bucket", (String.to_json f)))])
      
    let of_json j =
      {
        target_bucket =
          (Util.option_map (Json.lookup j "target_bucket") String.of_json);
        target_grants =
          (TargetGrants.of_json
             (Util.of_option_exn (Json.lookup j "target_grants")));
        target_prefix =
          (Util.option_map (Json.lookup j "target_prefix") String.of_json)
      } 
  end
module CompletedPartList =
  struct
    type t = CompletedPart.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map CompletedPart.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list CompletedPart.to_query v 
    let to_json v = `List (List.map CompletedPart.to_json v) 
    let of_json j = Json.to_list CompletedPart.of_json j 
  end
module Bucket =
  struct
    type t = {
      name: String.t ;
      creation_date: DateTime.t }
    let make ~name  ~creation_date  () = { name; creation_date } 
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          creation_date =
            (Xml.required "CreationDate"
               (Util.option_bind (Xml.member "CreationDate" xml)
                  DateTime.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CreationDate", (DateTime.to_query v.creation_date)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("creation_date", (DateTime.to_json v.creation_date));
           Some ("name", (String.to_json v.name))])
      
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        creation_date =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_date")))
      } 
  end
module CloudFunctionConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      event: Event.t option ;
      events: EventList.t ;
      cloud_function: String.t option ;
      invocation_role: String.t option }
    let make ?id  ?event  ?(events= [])  ?cloud_function  ?invocation_role 
      () = { id; event; events; cloud_function; invocation_role } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          event = (Util.option_bind (Xml.member "Event" xml) Event.parse);
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          cloud_function =
            (Util.option_bind (Xml.member "CloudFunction" xml) String.parse);
          invocation_role =
            (Util.option_bind (Xml.member "InvocationRole" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.invocation_role
              (fun f  -> Query.Pair ("InvocationRole", (String.to_query f)));
           Util.option_map v.cloud_function
             (fun f  -> Query.Pair ("CloudFunction", (String.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Util.option_map v.event
             (fun f  -> Query.Pair ("Event", (Event.to_query f)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.invocation_role
              (fun f  -> ("invocation_role", (String.to_json f)));
           Util.option_map v.cloud_function
             (fun f  -> ("cloud_function", (String.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Util.option_map v.event (fun f  -> ("event", (Event.to_json f)));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        event = (Util.option_map (Json.lookup j "event") Event.of_json);
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        cloud_function =
          (Util.option_map (Json.lookup j "cloud_function") String.of_json);
        invocation_role =
          (Util.option_map (Json.lookup j "invocation_role") String.of_json)
      } 
  end
module QueueConfigurationDeprecated =
  struct
    type t =
      {
      id: String.t option ;
      event: Event.t option ;
      events: EventList.t ;
      queue: String.t option }
    let make ?id  ?event  ?(events= [])  ?queue  () =
      { id; event; events; queue } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          event = (Util.option_bind (Xml.member "Event" xml) Event.parse);
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          queue = (Util.option_bind (Xml.member "Queue" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.queue
              (fun f  -> Query.Pair ("Queue", (String.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Util.option_map v.event
             (fun f  -> Query.Pair ("Event", (Event.to_query f)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.queue (fun f  -> ("queue", (String.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Util.option_map v.event (fun f  -> ("event", (Event.to_json f)));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        event = (Util.option_map (Json.lookup j "event") Event.of_json);
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        queue = (Util.option_map (Json.lookup j "queue") String.of_json)
      } 
  end
module TopicConfigurationDeprecated =
  struct
    type t =
      {
      id: String.t option ;
      events: EventList.t ;
      event: Event.t option ;
      topic: String.t option }
    let make ?id  ?(events= [])  ?event  ?topic  () =
      { id; events; event; topic } 
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Event" xml) EventList.parse));
          event = (Util.option_bind (Xml.member "Event" xml) Event.parse);
          topic = (Util.option_bind (Xml.member "Topic" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.topic
              (fun f  -> Query.Pair ("Topic", (String.to_query f)));
           Util.option_map v.event
             (fun f  -> Query.Pair ("Event", (Event.to_query f)));
           Some (Query.Pair ("Event", (EventList.to_query v.events)));
           Util.option_map v.id
             (fun f  -> Query.Pair ("Id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.topic (fun f  -> ("topic", (String.to_json f)));
           Util.option_map v.event (fun f  -> ("event", (Event.to_json f)));
           Some ("events", (EventList.to_json v.events));
           Util.option_map v.id (fun f  -> ("id", (String.to_json f)))])
      
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")));
        event = (Util.option_map (Json.lookup j "event") Event.of_json);
        topic = (Util.option_map (Json.lookup j "topic") String.of_json)
      } 
  end
module DeleteMarkerEntry =
  struct
    type t =
      {
      owner: Owner.t option ;
      key: String.t option ;
      version_id: String.t option ;
      is_latest: Boolean.t option ;
      last_modified: DateTime.t option }
    let make ?owner  ?key  ?version_id  ?is_latest  ?last_modified  () =
      { owner; key; version_id; is_latest; last_modified } 
    let parse xml =
      Some
        {
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse);
          is_latest =
            (Util.option_bind (Xml.member "IsLatest" xml) Boolean.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.is_latest
             (fun f  -> Query.Pair ("IsLatest", (Boolean.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.owner
             (fun f  -> Query.Pair ("Owner", (Owner.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.is_latest
             (fun f  -> ("is_latest", (Boolean.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)))])
      
    let of_json j =
      {
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        is_latest =
          (Util.option_map (Json.lookup j "is_latest") Boolean.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json)
      } 
  end
module ObjectVersion =
  struct
    type t =
      {
      e_tag: String.t option ;
      size: Integer.t option ;
      storage_class: ObjectVersionStorageClass.t option ;
      key: String.t option ;
      version_id: String.t option ;
      is_latest: Boolean.t option ;
      last_modified: DateTime.t option ;
      owner: Owner.t option }
    let make ?e_tag  ?size  ?storage_class  ?key  ?version_id  ?is_latest 
      ?last_modified  ?owner  () =
      {
        e_tag;
        size;
        storage_class;
        key;
        version_id;
        is_latest;
        last_modified;
        owner
      } 
    let parse xml =
      Some
        {
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          size = (Util.option_bind (Xml.member "Size" xml) Integer.parse);
          storage_class =
            (Util.option_bind (Xml.member "StorageClass" xml)
               ObjectVersionStorageClass.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse);
          is_latest =
            (Util.option_bind (Xml.member "IsLatest" xml) Boolean.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse);
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.owner
              (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Util.option_map v.last_modified
             (fun f  -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.is_latest
             (fun f  -> Query.Pair ("IsLatest", (Boolean.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair
                  ("StorageClass", (ObjectVersionStorageClass.to_query f)));
           Util.option_map v.size
             (fun f  -> Query.Pair ("Size", (Integer.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Util.option_map v.last_modified
             (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.is_latest
             (fun f  -> ("is_latest", (Boolean.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.storage_class
             (fun f  ->
                ("storage_class", (ObjectVersionStorageClass.to_json f)));
           Util.option_map v.size (fun f  -> ("size", (Integer.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)))])
      
    let of_json j =
      {
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        size = (Util.option_map (Json.lookup j "size") Integer.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             ObjectVersionStorageClass.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        is_latest =
          (Util.option_map (Json.lookup j "is_latest") Boolean.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json)
      } 
  end
module BucketVersioningStatus =
  struct
    type t =
      | Enabled 
      | Suspended 
    let str_to_t = [("Suspended", Suspended); ("Enabled", Enabled)] 
    let t_to_str = [(Suspended, "Suspended"); (Enabled, "Enabled")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module MFADelete =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)] 
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module Payer =
  struct
    type t =
      | Requester 
      | BucketOwner 
    let str_to_t = [("BucketOwner", BucketOwner); ("Requester", Requester)] 
    let t_to_str = [(BucketOwner, "BucketOwner"); (Requester, "Requester")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module DeletedObject =
  struct
    type t =
      {
      key: String.t option ;
      version_id: String.t option ;
      delete_marker: Boolean.t option ;
      delete_marker_version_id: String.t option }
    let make ?key  ?version_id  ?delete_marker  ?delete_marker_version_id  ()
      = { key; version_id; delete_marker; delete_marker_version_id } 
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse);
          delete_marker =
            (Util.option_bind (Xml.member "DeleteMarker" xml) Boolean.parse);
          delete_marker_version_id =
            (Util.option_bind (Xml.member "DeleteMarkerVersionId" xml)
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.delete_marker_version_id
              (fun f  ->
                 Query.Pair ("DeleteMarkerVersionId", (String.to_query f)));
           Util.option_map v.delete_marker
             (fun f  -> Query.Pair ("DeleteMarker", (Boolean.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.delete_marker_version_id
              (fun f  -> ("delete_marker_version_id", (String.to_json f)));
           Util.option_map v.delete_marker
             (fun f  -> ("delete_marker", (Boolean.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)))])
      
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        delete_marker =
          (Util.option_map (Json.lookup j "delete_marker") Boolean.of_json);
        delete_marker_version_id =
          (Util.option_map (Json.lookup j "delete_marker_version_id")
             String.of_json)
      } 
  end
module Error =
  struct
    type t =
      {
      key: String.t option ;
      version_id: String.t option ;
      code: String.t option ;
      message: String.t option }
    let make ?key  ?version_id  ?code  ?message  () =
      { key; version_id; code; message } 
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse);
          code = (Util.option_bind (Xml.member "Code" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f  -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.code
             (fun f  -> Query.Pair ("Code", (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f  -> ("message", (String.to_json f)));
           Util.option_map v.code (fun f  -> ("code", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)))])
      
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        code = (Util.option_map (Json.lookup j "code") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      } 
  end
module ObjectIdentifierList =
  struct
    type t = ObjectIdentifier.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map ObjectIdentifier.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list ObjectIdentifier.to_query v 
    let to_json v = `List (List.map ObjectIdentifier.to_json v) 
    let of_json j = Json.to_list ObjectIdentifier.of_json j 
  end
module Part =
  struct
    type t =
      {
      part_number: Integer.t option ;
      last_modified: DateTime.t option ;
      e_tag: String.t option ;
      size: Integer.t option }
    let make ?part_number  ?last_modified  ?e_tag  ?size  () =
      { part_number; last_modified; e_tag; size } 
    let parse xml =
      Some
        {
          part_number =
            (Util.option_bind (Xml.member "PartNumber" xml) Integer.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          size = (Util.option_bind (Xml.member "Size" xml) Integer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.size
              (fun f  -> Query.Pair ("Size", (Integer.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.last_modified
             (fun f  -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.part_number
             (fun f  -> Query.Pair ("PartNumber", (Integer.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.size (fun f  -> ("size", (Integer.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.last_modified
             (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.part_number
             (fun f  -> ("part_number", (Integer.to_json f)))])
      
    let of_json j =
      {
        part_number =
          (Util.option_map (Json.lookup j "part_number") Integer.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        size = (Util.option_map (Json.lookup j "size") Integer.of_json)
      } 
  end
module MultipartUpload =
  struct
    type t =
      {
      upload_id: String.t option ;
      key: String.t option ;
      initiated: DateTime.t option ;
      storage_class: StorageClass.t option ;
      owner: Owner.t option ;
      initiator: Initiator.t option }
    let make ?upload_id  ?key  ?initiated  ?storage_class  ?owner  ?initiator
       () = { upload_id; key; initiated; storage_class; owner; initiator } 
    let parse xml =
      Some
        {
          upload_id =
            (Util.option_bind (Xml.member "UploadId" xml) String.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          initiated =
            (Util.option_bind (Xml.member "Initiated" xml) DateTime.parse);
          storage_class =
            (Util.option_bind (Xml.member "StorageClass" xml)
               StorageClass.parse);
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse);
          initiator =
            (Util.option_bind (Xml.member "Initiator" xml) Initiator.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.initiator
              (fun f  -> Query.Pair ("Initiator", (Initiator.to_query f)));
           Util.option_map v.owner
             (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Util.option_map v.initiated
             (fun f  -> Query.Pair ("Initiated", (DateTime.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.upload_id
             (fun f  -> Query.Pair ("UploadId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.initiator
              (fun f  -> ("initiator", (Initiator.to_json f)));
           Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.initiated
             (fun f  -> ("initiated", (DateTime.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.upload_id
             (fun f  -> ("upload_id", (String.to_json f)))])
      
    let of_json j =
      {
        upload_id =
          (Util.option_map (Json.lookup j "upload_id") String.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        initiated =
          (Util.option_map (Json.lookup j "initiated") DateTime.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json);
        initiator =
          (Util.option_map (Json.lookup j "initiator") Initiator.of_json)
      } 
  end
module TagSet =
  struct
    type t = Tag.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "Tag" xml)) 
    let to_query v = Query.to_query_list Tag.to_query v 
    let to_json v = `List (List.map Tag.to_json v) 
    let of_json j = Json.to_list Tag.of_json j 
  end
module ErrorDocument =
  struct
    type t = {
      key: String.t }
    let make ~key  () = { key } 
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Key", (String.to_query v.key)))])
      
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("key", (String.to_json v.key))]) 
    let of_json j =
      { key = (String.of_json (Util.of_option_exn (Json.lookup j "key"))) } 
  end
module IndexDocument =
  struct
    type t = {
      suffix: String.t }
    let make ~suffix  () = { suffix } 
    let parse xml =
      Some
        {
          suffix =
            (Xml.required "Suffix"
               (Util.option_bind (Xml.member "Suffix" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Suffix", (String.to_query v.suffix)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("suffix", (String.to_json v.suffix))])
      
    let of_json j =
      {
        suffix =
          (String.of_json (Util.of_option_exn (Json.lookup j "suffix")))
      } 
  end
module RedirectAllRequestsTo =
  struct
    type t = {
      host_name: String.t ;
      protocol: Protocol.t option }
    let make ~host_name  ?protocol  () = { host_name; protocol } 
    let parse xml =
      Some
        {
          host_name =
            (Xml.required "HostName"
               (Util.option_bind (Xml.member "HostName" xml) String.parse));
          protocol =
            (Util.option_bind (Xml.member "Protocol" xml) Protocol.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.protocol
              (fun f  -> Query.Pair ("Protocol", (Protocol.to_query f)));
           Some (Query.Pair ("HostName", (String.to_query v.host_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.protocol
              (fun f  -> ("protocol", (Protocol.to_json f)));
           Some ("host_name", (String.to_json v.host_name))])
      
    let of_json j =
      {
        host_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "host_name")));
        protocol =
          (Util.option_map (Json.lookup j "protocol") Protocol.of_json)
      } 
  end
module RoutingRules =
  struct
    type t = RoutingRule.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map RoutingRule.parse (Xml.members "RoutingRule" xml))
      
    let to_query v = Query.to_query_list RoutingRule.to_query v 
    let to_json v = `List (List.map RoutingRule.to_json v) 
    let of_json j = Json.to_list RoutingRule.of_json j 
  end
module LambdaFunctionConfigurationList =
  struct
    type t = LambdaFunctionConfiguration.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map LambdaFunctionConfiguration.parse
           (Xml.members "member" xml))
      
    let to_query v =
      Query.to_query_list LambdaFunctionConfiguration.to_query v 
    let to_json v = `List (List.map LambdaFunctionConfiguration.to_json v) 
    let of_json j = Json.to_list LambdaFunctionConfiguration.of_json j 
  end
module QueueConfigurationList =
  struct
    type t = QueueConfiguration.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map QueueConfiguration.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list QueueConfiguration.to_query v 
    let to_json v = `List (List.map QueueConfiguration.to_json v) 
    let of_json j = Json.to_list QueueConfiguration.of_json j 
  end
module TopicConfigurationList =
  struct
    type t = TopicConfiguration.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map TopicConfiguration.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list TopicConfiguration.to_query v 
    let to_json v = `List (List.map TopicConfiguration.to_json v) 
    let of_json j = Json.to_list TopicConfiguration.of_json j 
  end
module CORSRules =
  struct
    type t = CORSRule.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map CORSRule.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list CORSRule.to_query v 
    let to_json v = `List (List.map CORSRule.to_json v) 
    let of_json j = Json.to_list CORSRule.of_json j 
  end
module RequestPayer =
  struct
    type t =
      | Requester 
    let str_to_t = [("requester", Requester)] 
    let t_to_str = [(Requester, "requester")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module RequestCharged =
  struct
    type t =
      | Requester 
    let str_to_t = [("requester", Requester)] 
    let t_to_str = [(Requester, "requester")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module ServerSideEncryption =
  struct
    type t =
      | AES256 
      | Aws_kms 
    let str_to_t = [("aws:kms", Aws_kms); ("AES256", AES256)] 
    let t_to_str = [(Aws_kms, "aws:kms"); (AES256, "AES256")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module ReplicationConfiguration =
  struct
    type t = {
      role: String.t ;
      rules: ReplicationRules.t }
    let make ~role  ~rules  () = { role; rules } 
    let parse xml =
      Some
        {
          role =
            (Xml.required "Role"
               (Util.option_bind (Xml.member "Role" xml) String.parse));
          rules =
            (Xml.required "Rule"
               (Util.option_bind (Xml.member "Rule" xml)
                  ReplicationRules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Rule", (ReplicationRules.to_query v.rules)));
           Some (Query.Pair ("Role", (String.to_query v.role)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("rules", (ReplicationRules.to_json v.rules));
           Some ("role", (String.to_json v.role))])
      
    let of_json j =
      {
        role = (String.of_json (Util.of_option_exn (Json.lookup j "role")));
        rules =
          (ReplicationRules.of_json
             (Util.of_option_exn (Json.lookup j "rules")))
      } 
  end
module CommonPrefixList =
  struct
    type t = CommonPrefix.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map CommonPrefix.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list CommonPrefix.to_query v 
    let to_json v = `List (List.map CommonPrefix.to_json v) 
    let of_json j = Json.to_list CommonPrefix.of_json j 
  end
module EncodingType =
  struct
    type t =
      | Url 
    let str_to_t = [("url", Url)] 
    let t_to_str = [(Url, "url")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module ObjectList =
  struct
    type t = Object.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Object.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list Object.to_query v 
    let to_json v = `List (List.map Object.to_json v) 
    let of_json j = Json.to_list Object.of_json j 
  end
module Metadata =
  struct
    type t = (String.t,String.t) Hashtbl.t
    let make elems () = elems 
    let parse xml = None 
    let to_query v = Query.to_query_hashtbl String.to_query v 
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k  -> fun v  -> fun acc  -> (k, (String.to_json v)) :: acc) v
           [])
      
    let of_json j = Json.to_hashtbl String.of_json j 
  end
module ReplicationStatus =
  struct
    type t =
      | COMPLETE 
      | PENDING 
      | FAILED 
      | REPLICA 
    let str_to_t =
      [("REPLICA", REPLICA);
      ("FAILED", FAILED);
      ("PENDING", PENDING);
      ("COMPLETE", COMPLETE)] 
    let t_to_str =
      [(REPLICA, "REPLICA");
      (FAILED, "FAILED");
      (PENDING, "PENDING");
      (COMPLETE, "COMPLETE")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module LifecycleConfiguration =
  struct
    type t = {
      rules: Rules.t }
    let make ~rules  () = { rules } 
    let parse xml =
      Some
        {
          rules =
            (Xml.required "Rule"
               (Util.option_bind (Xml.member "Rule" xml) Rules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Rule", (Rules.to_query v.rules)))])
      
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("rules", (Rules.to_json v.rules))]) 
    let of_json j =
      { rules = (Rules.of_json (Util.of_option_exn (Json.lookup j "rules")))
      } 
  end
module AccessControlPolicy =
  struct
    type t = {
      grants: Grants.t ;
      owner: Owner.t option }
    let make ?(grants= [])  ?owner  () = { grants; owner } 
    let parse xml =
      Some
        {
          grants =
            (Util.of_option []
               (Util.option_bind (Xml.member "AccessControlList" xml)
                  Grants.parse));
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.owner
              (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Some
             (Query.Pair ("AccessControlList", (Grants.to_query v.grants)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Some ("grants", (Grants.to_json v.grants))])
      
    let of_json j =
      {
        grants =
          (Grants.of_json (Util.of_option_exn (Json.lookup j "grants")));
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json)
      } 
  end
module ObjectCannedACL =
  struct
    type t =
      | Private 
      | Public_read 
      | Public_read_write 
      | Authenticated_read 
      | Bucket_owner_read 
      | Bucket_owner_full_control 
    let str_to_t =
      [("bucket-owner-full-control", Bucket_owner_full_control);
      ("bucket-owner-read", Bucket_owner_read);
      ("authenticated-read", Authenticated_read);
      ("public-read-write", Public_read_write);
      ("public-read", Public_read);
      ("private", Private)] 
    let t_to_str =
      [(Bucket_owner_full_control, "bucket-owner-full-control");
      (Bucket_owner_read, "bucket-owner-read");
      (Authenticated_read, "authenticated-read");
      (Public_read_write, "public-read-write");
      (Public_read, "public-read");
      (Private, "private")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module BucketCannedACL =
  struct
    type t =
      | Private 
      | Public_read 
      | Public_read_write 
      | Authenticated_read 
    let str_to_t =
      [("authenticated-read", Authenticated_read);
      ("public-read-write", Public_read_write);
      ("public-read", Public_read);
      ("private", Private)] 
    let t_to_str =
      [(Authenticated_read, "authenticated-read");
      (Public_read_write, "public-read-write");
      (Public_read, "public-read");
      (Private, "private")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module CreateBucketConfiguration =
  struct
    type t = {
      location_constraint: BucketLocationConstraint.t option }
    let make ?location_constraint  () = { location_constraint } 
    let parse xml =
      Some
        {
          location_constraint =
            (Util.option_bind (Xml.member "LocationConstraint" xml)
               BucketLocationConstraint.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.location_constraint
              (fun f  ->
                 Query.Pair
                   ("LocationConstraint",
                     (BucketLocationConstraint.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.location_constraint
              (fun f  ->
                 ("location_constraint",
                   (BucketLocationConstraint.to_json f)))])
      
    let of_json j =
      {
        location_constraint =
          (Util.option_map (Json.lookup j "location_constraint")
             BucketLocationConstraint.of_json)
      } 
  end
module BucketLoggingStatus =
  struct
    type t = {
      logging_enabled: LoggingEnabled.t option }
    let make ?logging_enabled  () = { logging_enabled } 
    let parse xml =
      Some
        {
          logging_enabled =
            (Util.option_bind (Xml.member "LoggingEnabled" xml)
               LoggingEnabled.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.logging_enabled
              (fun f  ->
                 Query.Pair ("LoggingEnabled", (LoggingEnabled.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.logging_enabled
              (fun f  -> ("logging_enabled", (LoggingEnabled.to_json f)))])
      
    let of_json j =
      {
        logging_enabled =
          (Util.option_map (Json.lookup j "logging_enabled")
             LoggingEnabled.of_json)
      } 
  end
module CompletedMultipartUpload =
  struct
    type t = {
      parts: CompletedPartList.t }
    let make ?(parts= [])  () = { parts } 
    let parse xml =
      Some
        {
          parts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Part" xml)
                  CompletedPartList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Part", (CompletedPartList.to_query v.parts)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parts", (CompletedPartList.to_json v.parts))])
      
    let of_json j =
      {
        parts =
          (CompletedPartList.of_json
             (Util.of_option_exn (Json.lookup j "parts")))
      } 
  end
module Buckets =
  struct
    type t = Bucket.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Bucket.parse (Xml.members "Bucket" xml)) 
    let to_query v = Query.to_query_list Bucket.to_query v 
    let to_json v = `List (List.map Bucket.to_json v) 
    let of_json j = Json.to_list Bucket.of_json j 
  end
module RestoreRequest =
  struct
    type t = {
      days: Integer.t }
    let make ~days  () = { days } 
    let parse xml =
      Some
        {
          days =
            (Xml.required "Days"
               (Util.option_bind (Xml.member "Days" xml) Integer.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Days", (Integer.to_query v.days)))])
      
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("days", (Integer.to_json v.days))]) 
    let of_json j =
      { days = (Integer.of_json (Util.of_option_exn (Json.lookup j "days")))
      } 
  end
module NotificationConfigurationDeprecated =
  struct
    type t =
      {
      topic_configuration: TopicConfigurationDeprecated.t option ;
      queue_configuration: QueueConfigurationDeprecated.t option ;
      cloud_function_configuration: CloudFunctionConfiguration.t option }
    let make ?topic_configuration  ?queue_configuration 
      ?cloud_function_configuration  () =
      {
        topic_configuration;
        queue_configuration;
        cloud_function_configuration
      } 
    let parse xml =
      Some
        {
          topic_configuration =
            (Util.option_bind (Xml.member "TopicConfiguration" xml)
               TopicConfigurationDeprecated.parse);
          queue_configuration =
            (Util.option_bind (Xml.member "QueueConfiguration" xml)
               QueueConfigurationDeprecated.parse);
          cloud_function_configuration =
            (Util.option_bind (Xml.member "CloudFunctionConfiguration" xml)
               CloudFunctionConfiguration.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_function_configuration
              (fun f  ->
                 Query.Pair
                   ("CloudFunctionConfiguration",
                     (CloudFunctionConfiguration.to_query f)));
           Util.option_map v.queue_configuration
             (fun f  ->
                Query.Pair
                  ("QueueConfiguration",
                    (QueueConfigurationDeprecated.to_query f)));
           Util.option_map v.topic_configuration
             (fun f  ->
                Query.Pair
                  ("TopicConfiguration",
                    (TopicConfigurationDeprecated.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_function_configuration
              (fun f  ->
                 ("cloud_function_configuration",
                   (CloudFunctionConfiguration.to_json f)));
           Util.option_map v.queue_configuration
             (fun f  ->
                ("queue_configuration",
                  (QueueConfigurationDeprecated.to_json f)));
           Util.option_map v.topic_configuration
             (fun f  ->
                ("topic_configuration",
                  (TopicConfigurationDeprecated.to_json f)))])
      
    let of_json j =
      {
        topic_configuration =
          (Util.option_map (Json.lookup j "topic_configuration")
             TopicConfigurationDeprecated.of_json);
        queue_configuration =
          (Util.option_map (Json.lookup j "queue_configuration")
             QueueConfigurationDeprecated.of_json);
        cloud_function_configuration =
          (Util.option_map (Json.lookup j "cloud_function_configuration")
             CloudFunctionConfiguration.of_json)
      } 
  end
module DeleteMarkers =
  struct
    type t = DeleteMarkerEntry.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map DeleteMarkerEntry.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list DeleteMarkerEntry.to_query v 
    let to_json v = `List (List.map DeleteMarkerEntry.to_json v) 
    let of_json j = Json.to_list DeleteMarkerEntry.of_json j 
  end
module ObjectVersionList =
  struct
    type t = ObjectVersion.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map ObjectVersion.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list ObjectVersion.to_query v 
    let to_json v = `List (List.map ObjectVersion.to_json v) 
    let of_json j = Json.to_list ObjectVersion.of_json j 
  end
module VersioningConfiguration =
  struct
    type t =
      {
      m_f_a_delete: MFADelete.t option ;
      status: BucketVersioningStatus.t option }
    let make ?m_f_a_delete  ?status  () = { m_f_a_delete; status } 
    let parse xml =
      Some
        {
          m_f_a_delete =
            (Util.option_bind (Xml.member "MfaDelete" xml) MFADelete.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               BucketVersioningStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f  ->
                 Query.Pair ("Status", (BucketVersioningStatus.to_query f)));
           Util.option_map v.m_f_a_delete
             (fun f  -> Query.Pair ("MfaDelete", (MFADelete.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f  -> ("status", (BucketVersioningStatus.to_json f)));
           Util.option_map v.m_f_a_delete
             (fun f  -> ("m_f_a_delete", (MFADelete.to_json f)))])
      
    let of_json j =
      {
        m_f_a_delete =
          (Util.option_map (Json.lookup j "m_f_a_delete") MFADelete.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             BucketVersioningStatus.of_json)
      } 
  end
module RequestPaymentConfiguration =
  struct
    type t = {
      payer: Payer.t }
    let make ~payer  () = { payer } 
    let parse xml =
      Some
        {
          payer =
            (Xml.required "Payer"
               (Util.option_bind (Xml.member "Payer" xml) Payer.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Payer", (Payer.to_query v.payer)))])
      
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("payer", (Payer.to_json v.payer))]) 
    let of_json j =
      { payer = (Payer.of_json (Util.of_option_exn (Json.lookup j "payer")))
      } 
  end
module DeletedObjects =
  struct
    type t = DeletedObject.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map DeletedObject.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list DeletedObject.to_query v 
    let to_json v = `List (List.map DeletedObject.to_json v) 
    let of_json j = Json.to_list DeletedObject.of_json j 
  end
module Errors =
  struct
    type t = Error.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Error.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list Error.to_query v 
    let to_json v = `List (List.map Error.to_json v) 
    let of_json j = Json.to_list Error.of_json j 
  end
module CopyPartResult =
  struct
    type t = {
      e_tag: String.t option ;
      last_modified: DateTime.t option }
    let make ?e_tag  ?last_modified  () = { e_tag; last_modified } 
    let parse xml =
      Some
        {
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)))])
      
    let of_json j =
      {
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json)
      } 
  end
module Delete =
  struct
    type t = {
      objects: ObjectIdentifierList.t ;
      quiet: Boolean.t option }
    let make ~objects  ?quiet  () = { objects; quiet } 
    let parse xml =
      Some
        {
          objects =
            (Xml.required "Object"
               (Util.option_bind (Xml.member "Object" xml)
                  ObjectIdentifierList.parse));
          quiet = (Util.option_bind (Xml.member "Quiet" xml) Boolean.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.quiet
              (fun f  -> Query.Pair ("Quiet", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Object", (ObjectIdentifierList.to_query v.objects)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.quiet
              (fun f  -> ("quiet", (Boolean.to_json f)));
           Some ("objects", (ObjectIdentifierList.to_json v.objects))])
      
    let of_json j =
      {
        objects =
          (ObjectIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "objects")));
        quiet = (Util.option_map (Json.lookup j "quiet") Boolean.of_json)
      } 
  end
module Parts =
  struct
    type t = Part.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all (List.map Part.parse (Xml.members "member" xml)) 
    let to_query v = Query.to_query_list Part.to_query v 
    let to_json v = `List (List.map Part.to_json v) 
    let of_json j = Json.to_list Part.of_json j 
  end
module MultipartUploadList =
  struct
    type t = MultipartUpload.t list
    let make elems () = elems 
    let parse xml =
      Util.option_all
        (List.map MultipartUpload.parse (Xml.members "member" xml))
      
    let to_query v = Query.to_query_list MultipartUpload.to_query v 
    let to_json v = `List (List.map MultipartUpload.to_json v) 
    let of_json j = Json.to_list MultipartUpload.of_json j 
  end
module MFADeleteStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)] 
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module MetadataDirective =
  struct
    type t =
      | COPY 
      | REPLACE 
    let str_to_t = [("REPLACE", REPLACE); ("COPY", COPY)] 
    let t_to_str = [(REPLACE, "REPLACE"); (COPY, "COPY")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
  end
module CopyObjectResult =
  struct
    type t = {
      e_tag: String.t option ;
      last_modified: DateTime.t option }
    let make ?e_tag  ?last_modified  () = { e_tag; last_modified } 
    let parse xml =
      Some
        {
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_modified
              (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)))])
      
    let of_json j =
      {
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json)
      } 
  end
module Tagging =
  struct
    type t = {
      tag_set: TagSet.t }
    let make ~tag_set  () = { tag_set } 
    let parse xml =
      Some
        {
          tag_set =
            (Xml.required "TagSet"
               (Util.option_bind (Xml.member "TagSet" xml) TagSet.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TagSet.member", (TagSet.to_query v.tag_set)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("tag_set", (TagSet.to_json v.tag_set))])
      
    let of_json j =
      {
        tag_set =
          (TagSet.of_json (Util.of_option_exn (Json.lookup j "tag_set")))
      } 
  end
module WebsiteConfiguration =
  struct
    type t =
      {
      error_document: ErrorDocument.t option ;
      index_document: IndexDocument.t option ;
      redirect_all_requests_to: RedirectAllRequestsTo.t option ;
      routing_rules: RoutingRules.t }
    let make ?error_document  ?index_document  ?redirect_all_requests_to 
      ?(routing_rules= [])  () =
      {
        error_document;
        index_document;
        redirect_all_requests_to;
        routing_rules
      } 
    let parse xml =
      Some
        {
          error_document =
            (Util.option_bind (Xml.member "ErrorDocument" xml)
               ErrorDocument.parse);
          index_document =
            (Util.option_bind (Xml.member "IndexDocument" xml)
               IndexDocument.parse);
          redirect_all_requests_to =
            (Util.option_bind (Xml.member "RedirectAllRequestsTo" xml)
               RedirectAllRequestsTo.parse);
          routing_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "RoutingRules" xml)
                  RoutingRules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RoutingRules.member",
                   (RoutingRules.to_query v.routing_rules)));
           Util.option_map v.redirect_all_requests_to
             (fun f  ->
                Query.Pair
                  ("RedirectAllRequestsTo",
                    (RedirectAllRequestsTo.to_query f)));
           Util.option_map v.index_document
             (fun f  ->
                Query.Pair ("IndexDocument", (IndexDocument.to_query f)));
           Util.option_map v.error_document
             (fun f  ->
                Query.Pair ("ErrorDocument", (ErrorDocument.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("routing_rules", (RoutingRules.to_json v.routing_rules));
           Util.option_map v.redirect_all_requests_to
             (fun f  ->
                ("redirect_all_requests_to",
                  (RedirectAllRequestsTo.to_json f)));
           Util.option_map v.index_document
             (fun f  -> ("index_document", (IndexDocument.to_json f)));
           Util.option_map v.error_document
             (fun f  -> ("error_document", (ErrorDocument.to_json f)))])
      
    let of_json j =
      {
        error_document =
          (Util.option_map (Json.lookup j "error_document")
             ErrorDocument.of_json);
        index_document =
          (Util.option_map (Json.lookup j "index_document")
             IndexDocument.of_json);
        redirect_all_requests_to =
          (Util.option_map (Json.lookup j "redirect_all_requests_to")
             RedirectAllRequestsTo.of_json);
        routing_rules =
          (RoutingRules.of_json
             (Util.of_option_exn (Json.lookup j "routing_rules")))
      } 
  end
module NotificationConfiguration =
  struct
    type t =
      {
      topic_configurations: TopicConfigurationList.t ;
      queue_configurations: QueueConfigurationList.t ;
      lambda_function_configurations: LambdaFunctionConfigurationList.t }
    let make ?(topic_configurations= [])  ?(queue_configurations= []) 
      ?(lambda_function_configurations= [])  () =
      {
        topic_configurations;
        queue_configurations;
        lambda_function_configurations
      } 
    let parse xml =
      Some
        {
          topic_configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "TopicConfiguration" xml)
                  TopicConfigurationList.parse));
          queue_configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "QueueConfiguration" xml)
                  QueueConfigurationList.parse));
          lambda_function_configurations =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "CloudFunctionConfiguration" xml)
                  LambdaFunctionConfigurationList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CloudFunctionConfiguration",
                   (LambdaFunctionConfigurationList.to_query
                      v.lambda_function_configurations)));
           Some
             (Query.Pair
                ("QueueConfiguration",
                  (QueueConfigurationList.to_query v.queue_configurations)));
           Some
             (Query.Pair
                ("TopicConfiguration",
                  (TopicConfigurationList.to_query v.topic_configurations)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("lambda_function_configurations",
                (LambdaFunctionConfigurationList.to_json
                   v.lambda_function_configurations));
           Some
             ("queue_configurations",
               (QueueConfigurationList.to_json v.queue_configurations));
           Some
             ("topic_configurations",
               (TopicConfigurationList.to_json v.topic_configurations))])
      
    let of_json j =
      {
        topic_configurations =
          (TopicConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "topic_configurations")));
        queue_configurations =
          (QueueConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "queue_configurations")));
        lambda_function_configurations =
          (LambdaFunctionConfigurationList.of_json
             (Util.of_option_exn
                (Json.lookup j "lambda_function_configurations")))
      } 
  end
module CORSConfiguration =
  struct
    type t = {
      c_o_r_s_rules: CORSRules.t }
    let make ?(c_o_r_s_rules= [])  () = { c_o_r_s_rules } 
    let parse xml =
      Some
        {
          c_o_r_s_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "CORSRule" xml) CORSRules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("CORSRule", (CORSRules.to_query v.c_o_r_s_rules)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("c_o_r_s_rules", (CORSRules.to_json v.c_o_r_s_rules))])
      
    let of_json j =
      {
        c_o_r_s_rules =
          (CORSRules.of_json
             (Util.of_option_exn (Json.lookup j "c_o_r_s_rules")))
      } 
  end
module DeleteBucketTaggingRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetObjectTorrentRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?request_payer  () =
      { bucket; key; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module CreateMultipartUploadOutput =
  struct
    type t =
      {
      bucket: String.t option ;
      key: String.t option ;
      upload_id: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?bucket  ?key  ?upload_id  ?server_side_encryption 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?request_charged  () =
      {
        bucket;
        key;
        upload_id;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          bucket = (Util.option_bind (Xml.member "Bucket" xml) String.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          upload_id =
            (Util.option_bind (Xml.member "UploadId" xml) String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.upload_id
             (fun f  -> Query.Pair ("UploadId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.bucket
             (fun f  -> Query.Pair ("Bucket", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.upload_id
             (fun f  -> ("upload_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.bucket
             (fun f  -> ("bucket", (String.to_json f)))])
      
    let of_json j =
      {
        bucket = (Util.option_map (Json.lookup j "bucket") String.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        upload_id =
          (Util.option_map (Json.lookup j "upload_id") String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module GetBucketLoggingRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetBucketReplicationOutput =
  struct
    type t = {
      replication_configuration: ReplicationConfiguration.t option }
    let make ?replication_configuration  () = { replication_configuration } 
    let parse xml =
      Some
        {
          replication_configuration =
            (Util.option_bind (Xml.member "ReplicationConfiguration" xml)
               ReplicationConfiguration.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_configuration
              (fun f  ->
                 Query.Pair
                   ("ReplicationConfiguration",
                     (ReplicationConfiguration.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_configuration
              (fun f  ->
                 ("replication_configuration",
                   (ReplicationConfiguration.to_json f)))])
      
    let of_json j =
      {
        replication_configuration =
          (Util.option_map (Json.lookup j "replication_configuration")
             ReplicationConfiguration.of_json)
      } 
  end
module AbortMultipartUploadOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged } 
    let parse xml =
      Some
        {
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)))])
      
    let of_json j =
      {
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module GetBucketWebsiteOutput =
  struct
    type t =
      {
      redirect_all_requests_to: RedirectAllRequestsTo.t option ;
      index_document: IndexDocument.t option ;
      error_document: ErrorDocument.t option ;
      routing_rules: RoutingRules.t }
    let make ?redirect_all_requests_to  ?index_document  ?error_document 
      ?(routing_rules= [])  () =
      {
        redirect_all_requests_to;
        index_document;
        error_document;
        routing_rules
      } 
    let parse xml =
      Some
        {
          redirect_all_requests_to =
            (Util.option_bind (Xml.member "RedirectAllRequestsTo" xml)
               RedirectAllRequestsTo.parse);
          index_document =
            (Util.option_bind (Xml.member "IndexDocument" xml)
               IndexDocument.parse);
          error_document =
            (Util.option_bind (Xml.member "ErrorDocument" xml)
               ErrorDocument.parse);
          routing_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "RoutingRules" xml)
                  RoutingRules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RoutingRules.member",
                   (RoutingRules.to_query v.routing_rules)));
           Util.option_map v.error_document
             (fun f  ->
                Query.Pair ("ErrorDocument", (ErrorDocument.to_query f)));
           Util.option_map v.index_document
             (fun f  ->
                Query.Pair ("IndexDocument", (IndexDocument.to_query f)));
           Util.option_map v.redirect_all_requests_to
             (fun f  ->
                Query.Pair
                  ("RedirectAllRequestsTo",
                    (RedirectAllRequestsTo.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("routing_rules", (RoutingRules.to_json v.routing_rules));
           Util.option_map v.error_document
             (fun f  -> ("error_document", (ErrorDocument.to_json f)));
           Util.option_map v.index_document
             (fun f  -> ("index_document", (IndexDocument.to_json f)));
           Util.option_map v.redirect_all_requests_to
             (fun f  ->
                ("redirect_all_requests_to",
                  (RedirectAllRequestsTo.to_json f)))])
      
    let of_json j =
      {
        redirect_all_requests_to =
          (Util.option_map (Json.lookup j "redirect_all_requests_to")
             RedirectAllRequestsTo.of_json);
        index_document =
          (Util.option_map (Json.lookup j "index_document")
             IndexDocument.of_json);
        error_document =
          (Util.option_map (Json.lookup j "error_document")
             ErrorDocument.of_json);
        routing_rules =
          (RoutingRules.of_json
             (Util.of_option_exn (Json.lookup j "routing_rules")))
      } 
  end
module GetObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      if_match: String.t option ;
      if_modified_since: DateTime.t option ;
      if_none_match: String.t option ;
      if_unmodified_since: DateTime.t option ;
      key: String.t ;
      range: String.t option ;
      response_cache_control: String.t option ;
      response_content_disposition: String.t option ;
      response_content_encoding: String.t option ;
      response_content_language: String.t option ;
      response_content_type: String.t option ;
      response_expires: DateTime.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ?if_match  ?if_modified_since  ?if_none_match 
      ?if_unmodified_since  ~key  ?range  ?response_cache_control 
      ?response_content_disposition  ?response_content_encoding 
      ?response_content_language  ?response_content_type  ?response_expires 
      ?version_id  ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  () =
      {
        bucket;
        if_match;
        if_modified_since;
        if_none_match;
        if_unmodified_since;
        key;
        range;
        response_cache_control;
        response_content_disposition;
        response_content_encoding;
        response_content_language;
        response_content_type;
        response_expires;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse);
          if_modified_since =
            (Util.option_bind (Xml.member "If-Modified-Since" xml)
               DateTime.parse);
          if_none_match =
            (Util.option_bind (Xml.member "If-None-Match" xml) String.parse);
          if_unmodified_since =
            (Util.option_bind (Xml.member "If-Unmodified-Since" xml)
               DateTime.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          range = (Util.option_bind (Xml.member "Range" xml) String.parse);
          response_cache_control =
            (Util.option_bind (Xml.member "response-cache-control" xml)
               String.parse);
          response_content_disposition =
            (Util.option_bind (Xml.member "response-content-disposition" xml)
               String.parse);
          response_content_encoding =
            (Util.option_bind (Xml.member "response-content-encoding" xml)
               String.parse);
          response_content_language =
            (Util.option_bind (Xml.member "response-content-language" xml)
               String.parse);
          response_content_type =
            (Util.option_bind (Xml.member "response-content-type" xml)
               String.parse);
          response_expires =
            (Util.option_bind (Xml.member "response-expires" xml)
               DateTime.parse);
          version_id =
            (Util.option_bind (Xml.member "versionId" xml) String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("versionId", (String.to_query f)));
           Util.option_map v.response_expires
             (fun f  ->
                Query.Pair ("response-expires", (DateTime.to_query f)));
           Util.option_map v.response_content_type
             (fun f  ->
                Query.Pair ("response-content-type", (String.to_query f)));
           Util.option_map v.response_content_language
             (fun f  ->
                Query.Pair ("response-content-language", (String.to_query f)));
           Util.option_map v.response_content_encoding
             (fun f  ->
                Query.Pair ("response-content-encoding", (String.to_query f)));
           Util.option_map v.response_content_disposition
             (fun f  ->
                Query.Pair
                  ("response-content-disposition", (String.to_query f)));
           Util.option_map v.response_cache_control
             (fun f  ->
                Query.Pair ("response-cache-control", (String.to_query f)));
           Util.option_map v.range
             (fun f  -> Query.Pair ("Range", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.if_unmodified_since
             (fun f  ->
                Query.Pair ("If-Unmodified-Since", (DateTime.to_query f)));
           Util.option_map v.if_none_match
             (fun f  -> Query.Pair ("If-None-Match", (String.to_query f)));
           Util.option_map v.if_modified_since
             (fun f  ->
                Query.Pair ("If-Modified-Since", (DateTime.to_query f)));
           Util.option_map v.if_match
             (fun f  -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.response_expires
             (fun f  -> ("response_expires", (DateTime.to_json f)));
           Util.option_map v.response_content_type
             (fun f  -> ("response_content_type", (String.to_json f)));
           Util.option_map v.response_content_language
             (fun f  -> ("response_content_language", (String.to_json f)));
           Util.option_map v.response_content_encoding
             (fun f  -> ("response_content_encoding", (String.to_json f)));
           Util.option_map v.response_content_disposition
             (fun f  -> ("response_content_disposition", (String.to_json f)));
           Util.option_map v.response_cache_control
             (fun f  -> ("response_cache_control", (String.to_json f)));
           Util.option_map v.range (fun f  -> ("range", (String.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.if_unmodified_since
             (fun f  -> ("if_unmodified_since", (DateTime.to_json f)));
           Util.option_map v.if_none_match
             (fun f  -> ("if_none_match", (String.to_json f)));
           Util.option_map v.if_modified_since
             (fun f  -> ("if_modified_since", (DateTime.to_json f)));
           Util.option_map v.if_match
             (fun f  -> ("if_match", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json);
        if_modified_since =
          (Util.option_map (Json.lookup j "if_modified_since")
             DateTime.of_json);
        if_none_match =
          (Util.option_map (Json.lookup j "if_none_match") String.of_json);
        if_unmodified_since =
          (Util.option_map (Json.lookup j "if_unmodified_since")
             DateTime.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        range = (Util.option_map (Json.lookup j "range") String.of_json);
        response_cache_control =
          (Util.option_map (Json.lookup j "response_cache_control")
             String.of_json);
        response_content_disposition =
          (Util.option_map (Json.lookup j "response_content_disposition")
             String.of_json);
        response_content_encoding =
          (Util.option_map (Json.lookup j "response_content_encoding")
             String.of_json);
        response_content_language =
          (Util.option_map (Json.lookup j "response_content_language")
             String.of_json);
        response_content_type =
          (Util.option_map (Json.lookup j "response_content_type")
             String.of_json);
        response_expires =
          (Util.option_map (Json.lookup j "response_expires")
             DateTime.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module HeadObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      if_match: String.t option ;
      if_modified_since: DateTime.t option ;
      if_none_match: String.t option ;
      if_unmodified_since: DateTime.t option ;
      key: String.t ;
      range: String.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ?if_match  ?if_modified_since  ?if_none_match 
      ?if_unmodified_since  ~key  ?range  ?version_id 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  () =
      {
        bucket;
        if_match;
        if_modified_since;
        if_none_match;
        if_unmodified_since;
        key;
        range;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          if_match =
            (Util.option_bind (Xml.member "If-Match" xml) String.parse);
          if_modified_since =
            (Util.option_bind (Xml.member "If-Modified-Since" xml)
               DateTime.parse);
          if_none_match =
            (Util.option_bind (Xml.member "If-None-Match" xml) String.parse);
          if_unmodified_since =
            (Util.option_bind (Xml.member "If-Unmodified-Since" xml)
               DateTime.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          range = (Util.option_bind (Xml.member "Range" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "versionId" xml) String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("versionId", (String.to_query f)));
           Util.option_map v.range
             (fun f  -> Query.Pair ("Range", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.if_unmodified_since
             (fun f  ->
                Query.Pair ("If-Unmodified-Since", (DateTime.to_query f)));
           Util.option_map v.if_none_match
             (fun f  -> Query.Pair ("If-None-Match", (String.to_query f)));
           Util.option_map v.if_modified_since
             (fun f  ->
                Query.Pair ("If-Modified-Since", (DateTime.to_query f)));
           Util.option_map v.if_match
             (fun f  -> Query.Pair ("If-Match", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.range (fun f  -> ("range", (String.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.if_unmodified_since
             (fun f  -> ("if_unmodified_since", (DateTime.to_json f)));
           Util.option_map v.if_none_match
             (fun f  -> ("if_none_match", (String.to_json f)));
           Util.option_map v.if_modified_since
             (fun f  -> ("if_modified_since", (DateTime.to_json f)));
           Util.option_map v.if_match
             (fun f  -> ("if_match", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        if_match =
          (Util.option_map (Json.lookup j "if_match") String.of_json);
        if_modified_since =
          (Util.option_map (Json.lookup j "if_modified_since")
             DateTime.of_json);
        if_none_match =
          (Util.option_map (Json.lookup j "if_none_match") String.of_json);
        if_unmodified_since =
          (Util.option_map (Json.lookup j "if_unmodified_since")
             DateTime.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        range = (Util.option_map (Json.lookup j "range") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module PutBucketReplicationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      replication_configuration: ReplicationConfiguration.t }
    let make ~bucket  ?content_m_d5  ~replication_configuration  () =
      { bucket; content_m_d5; replication_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          replication_configuration =
            (Xml.required "ReplicationConfiguration"
               (Util.option_bind (Xml.member "ReplicationConfiguration" xml)
                  ReplicationConfiguration.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReplicationConfiguration",
                   (ReplicationConfiguration.to_query
                      v.replication_configuration)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("replication_configuration",
                (ReplicationConfiguration.to_json v.replication_configuration));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        replication_configuration =
          (ReplicationConfiguration.of_json
             (Util.of_option_exn (Json.lookup j "replication_configuration")))
      } 
  end
module ListObjectsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      marker: String.t option ;
      next_marker: String.t option ;
      contents: ObjectList.t ;
      name: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      max_keys: Integer.t option ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option }
    let make ?is_truncated  ?marker  ?next_marker  ?(contents= [])  ?name 
      ?prefix  ?delimiter  ?max_keys  ?(common_prefixes= [])  ?encoding_type 
      () =
      {
        is_truncated;
        marker;
        next_marker;
        contents;
        name;
        prefix;
        delimiter;
        max_keys;
        common_prefixes;
        encoding_type
      } 
    let parse xml =
      Some
        {
          is_truncated =
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          contents =
            (Util.of_option []
               (Util.option_bind (Xml.member "Contents" xml) ObjectList.parse));
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          prefix = (Util.option_bind (Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Util.option_bind (Xml.member "Delimiter" xml) String.parse);
          max_keys =
            (Util.option_bind (Xml.member "MaxKeys" xml) Integer.parse);
          common_prefixes =
            (Util.of_option []
               (Util.option_bind (Xml.member "CommonPrefixes" xml)
                  CommonPrefixList.parse));
          encoding_type =
            (Util.option_bind (Xml.member "EncodingType" xml)
               EncodingType.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  ->
                 Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Util.option_map v.max_keys
             (fun f  -> Query.Pair ("MaxKeys", (Integer.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("Delimiter", (String.to_query f)));
           Util.option_map v.prefix
             (fun f  -> Query.Pair ("Prefix", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (String.to_query f)));
           Some
             (Query.Pair
                ("Contents.member", (ObjectList.to_query v.contents)));
           Util.option_map v.next_marker
             (fun f  -> Query.Pair ("NextMarker", (String.to_query f)));
           Util.option_map v.marker
             (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.is_truncated
             (fun f  -> Query.Pair ("IsTruncated", (Boolean.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Some
             ("common_prefixes",
               (CommonPrefixList.to_json v.common_prefixes));
           Util.option_map v.max_keys
             (fun f  -> ("max_keys", (Integer.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Util.option_map v.prefix
             (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.name (fun f  -> ("name", (String.to_json f)));
           Some ("contents", (ObjectList.to_json v.contents));
           Util.option_map v.next_marker
             (fun f  -> ("next_marker", (String.to_json f)));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.is_truncated
             (fun f  -> ("is_truncated", (Boolean.to_json f)))])
      
    let of_json j =
      {
        is_truncated =
          (Util.option_map (Json.lookup j "is_truncated") Boolean.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        contents =
          (ObjectList.of_json (Util.of_option_exn (Json.lookup j "contents")));
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json);
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        max_keys =
          (Util.option_map (Json.lookup j "max_keys") Integer.of_json);
        common_prefixes =
          (CommonPrefixList.of_json
             (Util.of_option_exn (Json.lookup j "common_prefixes")));
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json)
      } 
  end
module RestoreObjectOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged } 
    let parse xml =
      Some
        {
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)))])
      
    let of_json j =
      {
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module HeadObjectOutput =
  struct
    type t =
      {
      delete_marker: Boolean.t option ;
      accept_ranges: String.t option ;
      expiration: String.t option ;
      restore: String.t option ;
      last_modified: DateTime.t option ;
      content_length: Integer.t option ;
      e_tag: String.t option ;
      missing_meta: Integer.t option ;
      version_id: String.t option ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      website_redirect_location: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      metadata: Metadata.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option ;
      replication_status: ReplicationStatus.t option }
    let make ?delete_marker  ?accept_ranges  ?expiration  ?restore 
      ?last_modified  ?content_length  ?e_tag  ?missing_meta  ?version_id 
      ?cache_control  ?content_disposition  ?content_encoding 
      ?content_language  ?content_type  ?expires  ?website_redirect_location 
      ?server_side_encryption  ?metadata  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?storage_class 
      ?request_charged  ?replication_status  () =
      {
        delete_marker;
        accept_ranges;
        expiration;
        restore;
        last_modified;
        content_length;
        e_tag;
        missing_meta;
        version_id;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        expires;
        website_redirect_location;
        server_side_encryption;
        metadata;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        storage_class;
        request_charged;
        replication_status
      } 
    let parse xml =
      Some
        {
          delete_marker =
            (Util.option_bind (Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          accept_ranges =
            (Util.option_bind (Xml.member "accept-ranges" xml) String.parse);
          expiration =
            (Util.option_bind (Xml.member "x-amz-expiration" xml)
               String.parse);
          restore =
            (Util.option_bind (Xml.member "x-amz-restore" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "Last-Modified" xml) DateTime.parse);
          content_length =
            (Util.option_bind (Xml.member "Content-Length" xml) Integer.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          missing_meta =
            (Util.option_bind (Xml.member "x-amz-missing-meta" xml)
               Integer.parse);
          version_id =
            (Util.option_bind (Xml.member "x-amz-version-id" xml)
               String.parse);
          cache_control =
            (Util.option_bind (Xml.member "Cache-Control" xml) String.parse);
          content_disposition =
            (Util.option_bind (Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Util.option_bind (Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Util.option_bind (Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Util.option_bind (Xml.member "Content-Type" xml) String.parse);
          expires =
            (Util.option_bind (Xml.member "Expires" xml) DateTime.parse);
          website_redirect_location =
            (Util.option_bind
               (Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          metadata =
            (Util.option_bind (Xml.member "x-amz-meta-" xml) Metadata.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          storage_class =
            (Util.option_bind (Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          replication_status =
            (Util.option_bind (Xml.member "x-amz-replication-status" xml)
               ReplicationStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_status
              (fun f  ->
                 Query.Pair
                   ("x-amz-replication-status",
                     (ReplicationStatus.to_query f)));
           Util.option_map v.request_charged
             (fun f  ->
                Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("x-amz-storage-class", (StorageClass.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.metadata
             (fun f  -> Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.website_redirect_location
             (fun f  ->
                Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Util.option_map v.expires
             (fun f  -> Query.Pair ("Expires", (DateTime.to_query f)));
           Util.option_map v.content_type
             (fun f  -> Query.Pair ("Content-Type", (String.to_query f)));
           Util.option_map v.content_language
             (fun f  -> Query.Pair ("Content-Language", (String.to_query f)));
           Util.option_map v.content_encoding
             (fun f  -> Query.Pair ("Content-Encoding", (String.to_query f)));
           Util.option_map v.content_disposition
             (fun f  ->
                Query.Pair ("Content-Disposition", (String.to_query f)));
           Util.option_map v.cache_control
             (fun f  -> Query.Pair ("Cache-Control", (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("x-amz-version-id", (String.to_query f)));
           Util.option_map v.missing_meta
             (fun f  ->
                Query.Pair ("x-amz-missing-meta", (Integer.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.content_length
             (fun f  -> Query.Pair ("Content-Length", (Integer.to_query f)));
           Util.option_map v.last_modified
             (fun f  -> Query.Pair ("Last-Modified", (DateTime.to_query f)));
           Util.option_map v.restore
             (fun f  -> Query.Pair ("x-amz-restore", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  -> Query.Pair ("x-amz-expiration", (String.to_query f)));
           Util.option_map v.accept_ranges
             (fun f  -> Query.Pair ("accept-ranges", (String.to_query f)));
           Util.option_map v.delete_marker
             (fun f  ->
                Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_status
              (fun f  ->
                 ("replication_status", (ReplicationStatus.to_json f)));
           Util.option_map v.request_charged
             (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.metadata
             (fun f  -> ("metadata", (Metadata.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.website_redirect_location
             (fun f  -> ("website_redirect_location", (String.to_json f)));
           Util.option_map v.expires
             (fun f  -> ("expires", (DateTime.to_json f)));
           Util.option_map v.content_type
             (fun f  -> ("content_type", (String.to_json f)));
           Util.option_map v.content_language
             (fun f  -> ("content_language", (String.to_json f)));
           Util.option_map v.content_encoding
             (fun f  -> ("content_encoding", (String.to_json f)));
           Util.option_map v.content_disposition
             (fun f  -> ("content_disposition", (String.to_json f)));
           Util.option_map v.cache_control
             (fun f  -> ("cache_control", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.missing_meta
             (fun f  -> ("missing_meta", (Integer.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.content_length
             (fun f  -> ("content_length", (Integer.to_json f)));
           Util.option_map v.last_modified
             (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.restore
             (fun f  -> ("restore", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (String.to_json f)));
           Util.option_map v.accept_ranges
             (fun f  -> ("accept_ranges", (String.to_json f)));
           Util.option_map v.delete_marker
             (fun f  -> ("delete_marker", (Boolean.to_json f)))])
      
    let of_json j =
      {
        delete_marker =
          (Util.option_map (Json.lookup j "delete_marker") Boolean.of_json);
        accept_ranges =
          (Util.option_map (Json.lookup j "accept_ranges") String.of_json);
        expiration =
          (Util.option_map (Json.lookup j "expiration") String.of_json);
        restore = (Util.option_map (Json.lookup j "restore") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        content_length =
          (Util.option_map (Json.lookup j "content_length") Integer.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        missing_meta =
          (Util.option_map (Json.lookup j "missing_meta") Integer.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        cache_control =
          (Util.option_map (Json.lookup j "cache_control") String.of_json);
        content_disposition =
          (Util.option_map (Json.lookup j "content_disposition")
             String.of_json);
        content_encoding =
          (Util.option_map (Json.lookup j "content_encoding") String.of_json);
        content_language =
          (Util.option_map (Json.lookup j "content_language") String.of_json);
        content_type =
          (Util.option_map (Json.lookup j "content_type") String.of_json);
        expires =
          (Util.option_map (Json.lookup j "expires") DateTime.of_json);
        website_redirect_location =
          (Util.option_map (Json.lookup j "website_redirect_location")
             String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        metadata =
          (Util.option_map (Json.lookup j "metadata") Metadata.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json);
        replication_status =
          (Util.option_map (Json.lookup j "replication_status")
             ReplicationStatus.of_json)
      } 
  end
module GetBucketLifecycleOutput =
  struct
    type t = {
      rules: Rules.t }
    let make ?(rules= [])  () = { rules } 
    let parse xml =
      Some
        {
          rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "Rule" xml) Rules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Rule", (Rules.to_query v.rules)))])
      
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("rules", (Rules.to_json v.rules))]) 
    let of_json j =
      { rules = (Rules.of_json (Util.of_option_exn (Json.lookup j "rules")))
      } 
  end
module PutBucketLifecycleRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      lifecycle_configuration: LifecycleConfiguration.t option }
    let make ~bucket  ?content_m_d5  ?lifecycle_configuration  () =
      { bucket; content_m_d5; lifecycle_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          lifecycle_configuration =
            (Util.option_bind (Xml.member "LifecycleConfiguration" xml)
               LifecycleConfiguration.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.lifecycle_configuration
              (fun f  ->
                 Query.Pair
                   ("LifecycleConfiguration",
                     (LifecycleConfiguration.to_query f)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.lifecycle_configuration
              (fun f  ->
                 ("lifecycle_configuration",
                   (LifecycleConfiguration.to_json f)));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        lifecycle_configuration =
          (Util.option_map (Json.lookup j "lifecycle_configuration")
             LifecycleConfiguration.of_json)
      } 
  end
module PutObjectAclOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged } 
    let parse xml =
      Some
        {
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)))])
      
    let of_json j =
      {
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module ListPartsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      max_parts: Integer.t option ;
      part_number_marker: Integer.t option ;
      upload_id: String.t ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?max_parts  ?part_number_marker  ~upload_id 
      ?request_payer  () =
      { bucket; key; max_parts; part_number_marker; upload_id; request_payer
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          max_parts =
            (Util.option_bind (Xml.member "max-parts" xml) Integer.parse);
          part_number_marker =
            (Util.option_bind (Xml.member "part-number-marker" xml)
               Integer.parse);
          upload_id =
            (Xml.required "uploadId"
               (Util.option_bind (Xml.member "uploadId" xml) String.parse));
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Util.option_map v.part_number_marker
             (fun f  ->
                Query.Pair ("part-number-marker", (Integer.to_query f)));
           Util.option_map v.max_parts
             (fun f  -> Query.Pair ("max-parts", (Integer.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Some ("upload_id", (String.to_json v.upload_id));
           Util.option_map v.part_number_marker
             (fun f  -> ("part_number_marker", (Integer.to_json f)));
           Util.option_map v.max_parts
             (fun f  -> ("max_parts", (Integer.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        max_parts =
          (Util.option_map (Json.lookup j "max_parts") Integer.of_json);
        part_number_marker =
          (Util.option_map (Json.lookup j "part_number_marker")
             Integer.of_json);
        upload_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "upload_id")));
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetObjectTorrentOutput =
  struct
    type t = {
      body: Blob.t option ;
      request_charged: RequestCharged.t option }
    let make ?body  ?request_charged  () = { body; request_charged } 
    let parse xml =
      Some
        {
          body = (Util.option_bind (Xml.member "Body" xml) Blob.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.body
             (fun f  -> Query.Pair ("Body", (Blob.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.body (fun f  -> ("body", (Blob.to_json f)))])
      
    let of_json j =
      {
        body = (Util.option_map (Json.lookup j "body") Blob.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module PutObjectAclRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      access_control_policy: AccessControlPolicy.t option ;
      bucket: String.t ;
      content_m_d5: String.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      request_payer: RequestPayer.t option }
    let make ?a_c_l  ?access_control_policy  ~bucket  ?content_m_d5 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write 
      ?grant_write_a_c_p  ~key  ?request_payer  () =
      {
        a_c_l;
        access_control_policy;
        bucket;
        content_m_d5;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p;
        key;
        request_payer
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          access_control_policy =
            (Util.option_bind (Xml.member "AccessControlPolicy" xml)
               AccessControlPolicy.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Util.option_bind (Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.grant_write_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_write
             (fun f  -> Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.access_control_policy
             (fun f  ->
                Query.Pair
                  ("AccessControlPolicy", (AccessControlPolicy.to_query f)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.grant_write_a_c_p
             (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_write
             (fun f  -> ("grant_write", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.access_control_policy
             (fun f  ->
                ("access_control_policy", (AccessControlPolicy.to_json f)));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (ObjectCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") ObjectCannedACL.of_json);
        access_control_policy =
          (Util.option_map (Json.lookup j "access_control_policy")
             AccessControlPolicy.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write =
          (Util.option_map (Json.lookup j "grant_write") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module CreateBucketRequest =
  struct
    type t =
      {
      a_c_l: BucketCannedACL.t option ;
      bucket: String.t ;
      create_bucket_configuration: CreateBucketConfiguration.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option }
    let make ?a_c_l  ~bucket  ?create_bucket_configuration 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write 
      ?grant_write_a_c_p  () =
      {
        a_c_l;
        bucket;
        create_bucket_configuration;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               BucketCannedACL.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          create_bucket_configuration =
            (Util.option_bind (Xml.member "CreateBucketConfiguration" xml)
               CreateBucketConfiguration.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Util.option_bind (Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.grant_write_a_c_p
              (fun f  ->
                 Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_write
             (fun f  -> Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.create_bucket_configuration
             (fun f  ->
                Query.Pair
                  ("CreateBucketConfiguration",
                    (CreateBucketConfiguration.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (BucketCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.grant_write_a_c_p
              (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_write
             (fun f  -> ("grant_write", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.create_bucket_configuration
             (fun f  ->
                ("create_bucket_configuration",
                  (CreateBucketConfiguration.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (BucketCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") BucketCannedACL.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        create_bucket_configuration =
          (Util.option_map (Json.lookup j "create_bucket_configuration")
             CreateBucketConfiguration.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write =
          (Util.option_map (Json.lookup j "grant_write") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json)
      } 
  end
module HeadBucketRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module DeleteObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      m_f_a: String.t option ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?m_f_a  ?version_id  ?request_payer  () =
      { bucket; key; m_f_a; version_id; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          m_f_a =
            (Util.option_bind (Xml.member "x-amz-mfa" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "versionId" xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("versionId", (String.to_query f)));
           Util.option_map v.m_f_a
             (fun f  -> Query.Pair ("x-amz-mfa", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.m_f_a (fun f  -> ("m_f_a", (String.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        m_f_a = (Util.option_map (Json.lookup j "m_f_a") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetBucketTaggingRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module AbortMultipartUploadRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      upload_id: String.t ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ~upload_id  ?request_payer  () =
      { bucket; key; upload_id; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          upload_id =
            (Xml.required "uploadId"
               (Util.option_bind (Xml.member "uploadId" xml) String.parse));
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Some ("upload_id", (String.to_json v.upload_id));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        upload_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "upload_id")));
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetBucketLocationOutput =
  struct
    type t = {
      location_constraint: BucketLocationConstraint.t option }
    let make ?location_constraint  () = { location_constraint } 
    let parse xml =
      Some
        {
          location_constraint =
            (Util.option_bind (Xml.member "LocationConstraint" xml)
               BucketLocationConstraint.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.location_constraint
              (fun f  ->
                 Query.Pair
                   ("LocationConstraint",
                     (BucketLocationConstraint.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.location_constraint
              (fun f  ->
                 ("location_constraint",
                   (BucketLocationConstraint.to_json f)))])
      
    let of_json j =
      {
        location_constraint =
          (Util.option_map (Json.lookup j "location_constraint")
             BucketLocationConstraint.of_json)
      } 
  end
module GetBucketCorsOutput =
  struct
    type t = {
      c_o_r_s_rules: CORSRules.t }
    let make ?(c_o_r_s_rules= [])  () = { c_o_r_s_rules } 
    let parse xml =
      Some
        {
          c_o_r_s_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "CORSRule" xml) CORSRules.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("CORSRule", (CORSRules.to_query v.c_o_r_s_rules)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("c_o_r_s_rules", (CORSRules.to_json v.c_o_r_s_rules))])
      
    let of_json j =
      {
        c_o_r_s_rules =
          (CORSRules.of_json
             (Util.of_option_exn (Json.lookup j "c_o_r_s_rules")))
      } 
  end
module GetBucketPolicyRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module PutBucketLoggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      bucket_logging_status: BucketLoggingStatus.t ;
      content_m_d5: String.t option }
    let make ~bucket  ~bucket_logging_status  ?content_m_d5  () =
      { bucket; bucket_logging_status; content_m_d5 } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          bucket_logging_status =
            (Xml.required "BucketLoggingStatus"
               (Util.option_bind (Xml.member "BucketLoggingStatus" xml)
                  BucketLoggingStatus.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.content_m_d5
              (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some
             (Query.Pair
                ("BucketLoggingStatus",
                  (BucketLoggingStatus.to_query v.bucket_logging_status)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.content_m_d5
              (fun f  -> ("content_m_d5", (String.to_json f)));
           Some
             ("bucket_logging_status",
               (BucketLoggingStatus.to_json v.bucket_logging_status));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        bucket_logging_status =
          (BucketLoggingStatus.of_json
             (Util.of_option_exn (Json.lookup j "bucket_logging_status")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json)
      } 
  end
module UploadPartRequest =
  struct
    type t =
      {
      body: Blob.t option ;
      bucket: String.t ;
      content_length: Integer.t option ;
      content_m_d5: String.t option ;
      key: String.t ;
      part_number: Integer.t ;
      upload_id: String.t ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option }
    let make ?body  ~bucket  ?content_length  ?content_m_d5  ~key 
      ~part_number  ~upload_id  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?request_payer  () =
      {
        body;
        bucket;
        content_length;
        content_m_d5;
        key;
        part_number;
        upload_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer
      } 
    let parse xml =
      Some
        {
          body = (Util.option_bind (Xml.member "Body" xml) Blob.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_length =
            (Util.option_bind (Xml.member "Content-Length" xml) Integer.parse);
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          part_number =
            (Xml.required "partNumber"
               (Util.option_bind (Xml.member "partNumber" xml) Integer.parse));
          upload_id =
            (Xml.required "uploadId"
               (Util.option_bind (Xml.member "uploadId" xml) String.parse));
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Some (Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some (Query.Pair ("partNumber", (Integer.to_query v.part_number)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Util.option_map v.content_length
             (fun f  -> Query.Pair ("Content-Length", (Integer.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.body
             (fun f  -> Query.Pair ("Body", (Blob.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Some ("upload_id", (String.to_json v.upload_id));
           Some ("part_number", (Integer.to_json v.part_number));
           Some ("key", (String.to_json v.key));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Util.option_map v.content_length
             (fun f  -> ("content_length", (Integer.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.body (fun f  -> ("body", (Blob.to_json f)))])
      
    let of_json j =
      {
        body = (Util.option_map (Json.lookup j "body") Blob.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_length =
          (Util.option_map (Json.lookup j "content_length") Integer.of_json);
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        part_number =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "part_number")));
        upload_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "upload_id")));
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetBucketVersioningRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module CompleteMultipartUploadRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      multipart_upload: CompletedMultipartUpload.t option ;
      upload_id: String.t ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?multipart_upload  ~upload_id  ?request_payer  ()
      = { bucket; key; multipart_upload; upload_id; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          multipart_upload =
            (Util.option_bind (Xml.member "CompleteMultipartUpload" xml)
               CompletedMultipartUpload.parse);
          upload_id =
            (Xml.required "uploadId"
               (Util.option_bind (Xml.member "uploadId" xml) String.parse));
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Util.option_map v.multipart_upload
             (fun f  ->
                Query.Pair
                  ("CompleteMultipartUpload",
                    (CompletedMultipartUpload.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Some ("upload_id", (String.to_json v.upload_id));
           Util.option_map v.multipart_upload
             (fun f  ->
                ("multipart_upload", (CompletedMultipartUpload.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        multipart_upload =
          (Util.option_map (Json.lookup j "multipart_upload")
             CompletedMultipartUpload.of_json);
        upload_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "upload_id")));
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module DeleteBucketPolicyRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetBucketReplicationRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module NoSuchBucket =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module DeleteBucketRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module ObjectAlreadyInActiveTierError =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module ListBucketsOutput =
  struct
    type t = {
      buckets: Buckets.t ;
      owner: Owner.t option }
    let make ?(buckets= [])  ?owner  () = { buckets; owner } 
    let parse xml =
      Some
        {
          buckets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Buckets" xml) Buckets.parse));
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.owner
              (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Some (Query.Pair ("Buckets.member", (Buckets.to_query v.buckets)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Some ("buckets", (Buckets.to_json v.buckets))])
      
    let of_json j =
      {
        buckets =
          (Buckets.of_json (Util.of_option_exn (Json.lookup j "buckets")));
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json)
      } 
  end
module GetBucketLocationRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module BucketAlreadyExists =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module CreateMultipartUploadRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_payer: RequestPayer.t option }
    let make ?a_c_l  ~bucket  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_type  ?expires 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write_a_c_p
       ~key  ?metadata  ?server_side_encryption  ?storage_class 
      ?website_redirect_location  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?request_payer  () =
      {
        a_c_l;
        bucket;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_payer
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          cache_control =
            (Util.option_bind (Xml.member "Cache-Control" xml) String.parse);
          content_disposition =
            (Util.option_bind (Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Util.option_bind (Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Util.option_bind (Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Util.option_bind (Xml.member "Content-Type" xml) String.parse);
          expires =
            (Util.option_bind (Xml.member "Expires" xml) DateTime.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          metadata =
            (Util.option_bind (Xml.member "x-amz-meta-" xml) Metadata.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Util.option_bind (Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Util.option_bind
               (Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.website_redirect_location
             (fun f  ->
                Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("x-amz-storage-class", (StorageClass.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.metadata
             (fun f  -> Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.grant_write_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.expires
             (fun f  -> Query.Pair ("Expires", (DateTime.to_query f)));
           Util.option_map v.content_type
             (fun f  -> Query.Pair ("Content-Type", (String.to_query f)));
           Util.option_map v.content_language
             (fun f  -> Query.Pair ("Content-Language", (String.to_query f)));
           Util.option_map v.content_encoding
             (fun f  -> Query.Pair ("Content-Encoding", (String.to_query f)));
           Util.option_map v.content_disposition
             (fun f  ->
                Query.Pair ("Content-Disposition", (String.to_query f)));
           Util.option_map v.cache_control
             (fun f  -> Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.website_redirect_location
             (fun f  -> ("website_redirect_location", (String.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.metadata
             (fun f  -> ("metadata", (Metadata.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.grant_write_a_c_p
             (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.expires
             (fun f  -> ("expires", (DateTime.to_json f)));
           Util.option_map v.content_type
             (fun f  -> ("content_type", (String.to_json f)));
           Util.option_map v.content_language
             (fun f  -> ("content_language", (String.to_json f)));
           Util.option_map v.content_encoding
             (fun f  -> ("content_encoding", (String.to_json f)));
           Util.option_map v.content_disposition
             (fun f  -> ("content_disposition", (String.to_json f)));
           Util.option_map v.cache_control
             (fun f  -> ("cache_control", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (ObjectCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") ObjectCannedACL.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        cache_control =
          (Util.option_map (Json.lookup j "cache_control") String.of_json);
        content_disposition =
          (Util.option_map (Json.lookup j "content_disposition")
             String.of_json);
        content_encoding =
          (Util.option_map (Json.lookup j "content_encoding") String.of_json);
        content_language =
          (Util.option_map (Json.lookup j "content_language") String.of_json);
        content_type =
          (Util.option_map (Json.lookup j "content_type") String.of_json);
        expires =
          (Util.option_map (Json.lookup j "expires") DateTime.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        metadata =
          (Util.option_map (Json.lookup j "metadata") Metadata.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        website_redirect_location =
          (Util.option_map (Json.lookup j "website_redirect_location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetBucketPolicyOutput =
  struct
    type t = {
      policy: String.t option }
    let make ?policy  () = { policy } 
    let parse xml =
      Some
        { policy = (Util.option_bind (Xml.member "Policy" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f  -> Query.Pair ("Policy", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f  -> ("policy", (String.to_json f)))])
      
    let of_json j =
      { policy = (Util.option_map (Json.lookup j "policy") String.of_json) } 
  end
module RestoreObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      restore_request: RestoreRequest.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?version_id  ?restore_request  ?request_payer  ()
      = { bucket; key; version_id; restore_request; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          version_id =
            (Util.option_bind (Xml.member "versionId" xml) String.parse);
          restore_request =
            (Util.option_bind (Xml.member "RestoreRequest" xml)
               RestoreRequest.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.restore_request
             (fun f  ->
                Query.Pair ("RestoreRequest", (RestoreRequest.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("versionId", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.restore_request
             (fun f  -> ("restore_request", (RestoreRequest.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        restore_request =
          (Util.option_map (Json.lookup j "restore_request")
             RestoreRequest.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module PutBucketPolicyRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      policy: String.t }
    let make ~bucket  ?content_m_d5  ~policy  () =
      { bucket; content_m_d5; policy } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          policy =
            (Xml.required "Policy"
               (Util.option_bind (Xml.member "Policy" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Policy", (String.to_query v.policy)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy", (String.to_json v.policy));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        policy =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy")))
      } 
  end
module PutBucketNotificationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      notification_configuration: NotificationConfigurationDeprecated.t }
    let make ~bucket  ?content_m_d5  ~notification_configuration  () =
      { bucket; content_m_d5; notification_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          notification_configuration =
            (Xml.required "NotificationConfiguration"
               (Util.option_bind (Xml.member "NotificationConfiguration" xml)
                  NotificationConfigurationDeprecated.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NotificationConfiguration",
                   (NotificationConfigurationDeprecated.to_query
                      v.notification_configuration)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("notification_configuration",
                (NotificationConfigurationDeprecated.to_json
                   v.notification_configuration));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        notification_configuration =
          (NotificationConfigurationDeprecated.of_json
             (Util.of_option_exn (Json.lookup j "notification_configuration")))
      } 
  end
module PutObjectOutput =
  struct
    type t =
      {
      expiration: String.t option ;
      e_tag: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?expiration  ?e_tag  ?server_side_encryption  ?version_id 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?request_charged  () =
      {
        expiration;
        e_tag;
        server_side_encryption;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          expiration =
            (Util.option_bind (Xml.member "x-amz-expiration" xml)
               String.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          version_id =
            (Util.option_bind (Xml.member "x-amz-version-id" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("x-amz-version-id", (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  -> Query.Pair ("x-amz-expiration", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (String.to_json f)))])
      
    let of_json j =
      {
        expiration =
          (Util.option_map (Json.lookup j "expiration") String.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module GetBucketTaggingOutput =
  struct
    type t = {
      tag_set: TagSet.t }
    let make ~tag_set  () = { tag_set } 
    let parse xml =
      Some
        {
          tag_set =
            (Xml.required "TagSet"
               (Util.option_bind (Xml.member "TagSet" xml) TagSet.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("TagSet.member", (TagSet.to_query v.tag_set)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("tag_set", (TagSet.to_json v.tag_set))])
      
    let of_json j =
      {
        tag_set =
          (TagSet.of_json (Util.of_option_exn (Json.lookup j "tag_set")))
      } 
  end
module ListObjectVersionsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      key_marker: String.t option ;
      version_id_marker: String.t option ;
      next_key_marker: String.t option ;
      next_version_id_marker: String.t option ;
      versions: ObjectVersionList.t ;
      delete_markers: DeleteMarkers.t ;
      name: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      max_keys: Integer.t option ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option }
    let make ?is_truncated  ?key_marker  ?version_id_marker  ?next_key_marker
       ?next_version_id_marker  ?(versions= [])  ?(delete_markers= [])  ?name
       ?prefix  ?delimiter  ?max_keys  ?(common_prefixes= [])  ?encoding_type
       () =
      {
        is_truncated;
        key_marker;
        version_id_marker;
        next_key_marker;
        next_version_id_marker;
        versions;
        delete_markers;
        name;
        prefix;
        delimiter;
        max_keys;
        common_prefixes;
        encoding_type
      } 
    let parse xml =
      Some
        {
          is_truncated =
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse);
          key_marker =
            (Util.option_bind (Xml.member "KeyMarker" xml) String.parse);
          version_id_marker =
            (Util.option_bind (Xml.member "VersionIdMarker" xml) String.parse);
          next_key_marker =
            (Util.option_bind (Xml.member "NextKeyMarker" xml) String.parse);
          next_version_id_marker =
            (Util.option_bind (Xml.member "NextVersionIdMarker" xml)
               String.parse);
          versions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Version" xml)
                  ObjectVersionList.parse));
          delete_markers =
            (Util.of_option []
               (Util.option_bind (Xml.member "DeleteMarker" xml)
                  DeleteMarkers.parse));
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          prefix = (Util.option_bind (Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Util.option_bind (Xml.member "Delimiter" xml) String.parse);
          max_keys =
            (Util.option_bind (Xml.member "MaxKeys" xml) Integer.parse);
          common_prefixes =
            (Util.of_option []
               (Util.option_bind (Xml.member "CommonPrefixes" xml)
                  CommonPrefixList.parse));
          encoding_type =
            (Util.option_bind (Xml.member "EncodingType" xml)
               EncodingType.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  ->
                 Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Util.option_map v.max_keys
             (fun f  -> Query.Pair ("MaxKeys", (Integer.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("Delimiter", (String.to_query f)));
           Util.option_map v.prefix
             (fun f  -> Query.Pair ("Prefix", (String.to_query f)));
           Util.option_map v.name
             (fun f  -> Query.Pair ("Name", (String.to_query f)));
           Some
             (Query.Pair
                ("DeleteMarker", (DeleteMarkers.to_query v.delete_markers)));
           Some
             (Query.Pair ("Version", (ObjectVersionList.to_query v.versions)));
           Util.option_map v.next_version_id_marker
             (fun f  ->
                Query.Pair ("NextVersionIdMarker", (String.to_query f)));
           Util.option_map v.next_key_marker
             (fun f  -> Query.Pair ("NextKeyMarker", (String.to_query f)));
           Util.option_map v.version_id_marker
             (fun f  -> Query.Pair ("VersionIdMarker", (String.to_query f)));
           Util.option_map v.key_marker
             (fun f  -> Query.Pair ("KeyMarker", (String.to_query f)));
           Util.option_map v.is_truncated
             (fun f  -> Query.Pair ("IsTruncated", (Boolean.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Some
             ("common_prefixes",
               (CommonPrefixList.to_json v.common_prefixes));
           Util.option_map v.max_keys
             (fun f  -> ("max_keys", (Integer.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Util.option_map v.prefix
             (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.name (fun f  -> ("name", (String.to_json f)));
           Some ("delete_markers", (DeleteMarkers.to_json v.delete_markers));
           Some ("versions", (ObjectVersionList.to_json v.versions));
           Util.option_map v.next_version_id_marker
             (fun f  -> ("next_version_id_marker", (String.to_json f)));
           Util.option_map v.next_key_marker
             (fun f  -> ("next_key_marker", (String.to_json f)));
           Util.option_map v.version_id_marker
             (fun f  -> ("version_id_marker", (String.to_json f)));
           Util.option_map v.key_marker
             (fun f  -> ("key_marker", (String.to_json f)));
           Util.option_map v.is_truncated
             (fun f  -> ("is_truncated", (Boolean.to_json f)))])
      
    let of_json j =
      {
        is_truncated =
          (Util.option_map (Json.lookup j "is_truncated") Boolean.of_json);
        key_marker =
          (Util.option_map (Json.lookup j "key_marker") String.of_json);
        version_id_marker =
          (Util.option_map (Json.lookup j "version_id_marker") String.of_json);
        next_key_marker =
          (Util.option_map (Json.lookup j "next_key_marker") String.of_json);
        next_version_id_marker =
          (Util.option_map (Json.lookup j "next_version_id_marker")
             String.of_json);
        versions =
          (ObjectVersionList.of_json
             (Util.of_option_exn (Json.lookup j "versions")));
        delete_markers =
          (DeleteMarkers.of_json
             (Util.of_option_exn (Json.lookup j "delete_markers")));
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json);
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        max_keys =
          (Util.option_map (Json.lookup j "max_keys") Integer.of_json);
        common_prefixes =
          (CommonPrefixList.of_json
             (Util.of_option_exn (Json.lookup j "common_prefixes")));
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json)
      } 
  end
module ObjectNotInActiveTierError =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module PutBucketVersioningRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      m_f_a: String.t option ;
      versioning_configuration: VersioningConfiguration.t }
    let make ~bucket  ?content_m_d5  ?m_f_a  ~versioning_configuration  () =
      { bucket; content_m_d5; m_f_a; versioning_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          m_f_a =
            (Util.option_bind (Xml.member "x-amz-mfa" xml) String.parse);
          versioning_configuration =
            (Xml.required "VersioningConfiguration"
               (Util.option_bind (Xml.member "VersioningConfiguration" xml)
                  VersioningConfiguration.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("VersioningConfiguration",
                   (VersioningConfiguration.to_query
                      v.versioning_configuration)));
           Util.option_map v.m_f_a
             (fun f  -> Query.Pair ("x-amz-mfa", (String.to_query f)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("versioning_configuration",
                (VersioningConfiguration.to_json v.versioning_configuration));
           Util.option_map v.m_f_a (fun f  -> ("m_f_a", (String.to_json f)));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        m_f_a = (Util.option_map (Json.lookup j "m_f_a") String.of_json);
        versioning_configuration =
          (VersioningConfiguration.of_json
             (Util.of_option_exn (Json.lookup j "versioning_configuration")))
      } 
  end
module PutBucketRequestPaymentRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      request_payment_configuration: RequestPaymentConfiguration.t }
    let make ~bucket  ?content_m_d5  ~request_payment_configuration  () =
      { bucket; content_m_d5; request_payment_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          request_payment_configuration =
            (Xml.required "RequestPaymentConfiguration"
               (Util.option_bind
                  (Xml.member "RequestPaymentConfiguration" xml)
                  RequestPaymentConfiguration.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RequestPaymentConfiguration",
                   (RequestPaymentConfiguration.to_query
                      v.request_payment_configuration)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("request_payment_configuration",
                (RequestPaymentConfiguration.to_json
                   v.request_payment_configuration));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        request_payment_configuration =
          (RequestPaymentConfiguration.of_json
             (Util.of_option_exn
                (Json.lookup j "request_payment_configuration")))
      } 
  end
module NoSuchKey =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module DeleteObjectsOutput =
  struct
    type t =
      {
      deleted: DeletedObjects.t ;
      request_charged: RequestCharged.t option ;
      errors: Errors.t }
    let make ?(deleted= [])  ?request_charged  ?(errors= [])  () =
      { deleted; request_charged; errors } 
    let parse xml =
      Some
        {
          deleted =
            (Util.of_option []
               (Util.option_bind (Xml.member "Deleted" xml)
                  DeletedObjects.parse));
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          errors =
            (Util.of_option []
               (Util.option_bind (Xml.member "Error" xml) Errors.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Error", (Errors.to_query v.errors)));
           Util.option_map v.request_charged
             (fun f  ->
                Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Some
             (Query.Pair
                ("Deleted.member", (DeletedObjects.to_query v.deleted)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("errors", (Errors.to_json v.errors));
           Util.option_map v.request_charged
             (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Some ("deleted", (DeletedObjects.to_json v.deleted))])
      
    let of_json j =
      {
        deleted =
          (DeletedObjects.of_json
             (Util.of_option_exn (Json.lookup j "deleted")));
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json);
        errors =
          (Errors.of_json (Util.of_option_exn (Json.lookup j "errors")))
      } 
  end
module DeleteObjectOutput =
  struct
    type t =
      {
      delete_marker: Boolean.t option ;
      version_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?delete_marker  ?version_id  ?request_charged  () =
      { delete_marker; version_id; request_charged } 
    let parse xml =
      Some
        {
          delete_marker =
            (Util.option_bind (Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          version_id =
            (Util.option_bind (Xml.member "x-amz-version-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("x-amz-version-id", (String.to_query f)));
           Util.option_map v.delete_marker
             (fun f  ->
                Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.delete_marker
             (fun f  -> ("delete_marker", (Boolean.to_json f)))])
      
    let of_json j =
      {
        delete_marker =
          (Util.option_map (Json.lookup j "delete_marker") Boolean.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module DeleteBucketReplicationRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module UploadPartCopyOutput =
  struct
    type t =
      {
      copy_source_version_id: String.t option ;
      copy_part_result: CopyPartResult.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?copy_source_version_id  ?copy_part_result 
      ?server_side_encryption  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?request_charged  () =
      {
        copy_source_version_id;
        copy_part_result;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          copy_source_version_id =
            (Util.option_bind (Xml.member "x-amz-copy-source-version-id" xml)
               String.parse);
          copy_part_result =
            (Util.option_bind (Xml.member "CopyPartResult" xml)
               CopyPartResult.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.copy_part_result
             (fun f  ->
                Query.Pair ("CopyPartResult", (CopyPartResult.to_query f)));
           Util.option_map v.copy_source_version_id
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-version-id", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.copy_part_result
             (fun f  -> ("copy_part_result", (CopyPartResult.to_json f)));
           Util.option_map v.copy_source_version_id
             (fun f  -> ("copy_source_version_id", (String.to_json f)))])
      
    let of_json j =
      {
        copy_source_version_id =
          (Util.option_map (Json.lookup j "copy_source_version_id")
             String.of_json);
        copy_part_result =
          (Util.option_map (Json.lookup j "copy_part_result")
             CopyPartResult.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module ListObjectsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      marker: String.t option ;
      max_keys: Integer.t option ;
      prefix: String.t option }
    let make ~bucket  ?delimiter  ?encoding_type  ?marker  ?max_keys  ?prefix
       () = { bucket; delimiter; encoding_type; marker; max_keys; prefix } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          delimiter =
            (Util.option_bind (Xml.member "delimiter" xml) String.parse);
          encoding_type =
            (Util.option_bind (Xml.member "encoding-type" xml)
               EncodingType.parse);
          marker = (Util.option_bind (Xml.member "marker" xml) String.parse);
          max_keys =
            (Util.option_bind (Xml.member "max-keys" xml) Integer.parse);
          prefix = (Util.option_bind (Xml.member "prefix" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.prefix
              (fun f  -> Query.Pair ("prefix", (String.to_query f)));
           Util.option_map v.max_keys
             (fun f  -> Query.Pair ("max-keys", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f  -> Query.Pair ("marker", (String.to_query f)));
           Util.option_map v.encoding_type
             (fun f  ->
                Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("delimiter", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.prefix
              (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.max_keys
             (fun f  -> ("max_keys", (Integer.to_json f)));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.encoding_type
             (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_keys =
          (Util.option_map (Json.lookup j "max_keys") Integer.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json)
      } 
  end
module CompleteMultipartUploadOutput =
  struct
    type t =
      {
      location: String.t option ;
      bucket: String.t option ;
      key: String.t option ;
      expiration: String.t option ;
      e_tag: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      version_id: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?location  ?bucket  ?key  ?expiration  ?e_tag 
      ?server_side_encryption  ?version_id  ?s_s_e_k_m_s_key_id 
      ?request_charged  () =
      {
        location;
        bucket;
        key;
        expiration;
        e_tag;
        server_side_encryption;
        version_id;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse);
          bucket = (Util.option_bind (Xml.member "Bucket" xml) String.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          expiration =
            (Util.option_bind (Xml.member "x-amz-expiration" xml)
               String.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          version_id =
            (Util.option_bind (Xml.member "x-amz-version-id" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("x-amz-version-id", (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  -> Query.Pair ("x-amz-expiration", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.bucket
             (fun f  -> Query.Pair ("Bucket", (String.to_query f)));
           Util.option_map v.location
             (fun f  -> Query.Pair ("Location", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.bucket
             (fun f  -> ("bucket", (String.to_json f)));
           Util.option_map v.location
             (fun f  -> ("location", (String.to_json f)))])
      
    let of_json j =
      {
        location =
          (Util.option_map (Json.lookup j "location") String.of_json);
        bucket = (Util.option_map (Json.lookup j "bucket") String.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        expiration =
          (Util.option_map (Json.lookup j "expiration") String.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module PutObjectRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      body: Blob.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_length: Integer.t option ;
      content_m_d5: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_payer: RequestPayer.t option }
    let make ?a_c_l  ?body  ~bucket  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_length  ?content_m_d5 
      ?content_type  ?expires  ?grant_full_control  ?grant_read 
      ?grant_read_a_c_p  ?grant_write_a_c_p  ~key  ?metadata 
      ?server_side_encryption  ?storage_class  ?website_redirect_location 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?request_payer  () =
      {
        a_c_l;
        body;
        bucket;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_length;
        content_m_d5;
        content_type;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_payer
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          body = (Util.option_bind (Xml.member "Body" xml) Blob.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          cache_control =
            (Util.option_bind (Xml.member "Cache-Control" xml) String.parse);
          content_disposition =
            (Util.option_bind (Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Util.option_bind (Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Util.option_bind (Xml.member "Content-Language" xml)
               String.parse);
          content_length =
            (Util.option_bind (Xml.member "Content-Length" xml) Integer.parse);
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          content_type =
            (Util.option_bind (Xml.member "Content-Type" xml) String.parse);
          expires =
            (Util.option_bind (Xml.member "Expires" xml) DateTime.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          metadata =
            (Util.option_bind (Xml.member "x-amz-meta-" xml) Metadata.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Util.option_bind (Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Util.option_bind
               (Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.website_redirect_location
             (fun f  ->
                Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("x-amz-storage-class", (StorageClass.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.metadata
             (fun f  -> Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.grant_write_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.expires
             (fun f  -> Query.Pair ("Expires", (DateTime.to_query f)));
           Util.option_map v.content_type
             (fun f  -> Query.Pair ("Content-Type", (String.to_query f)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Util.option_map v.content_length
             (fun f  -> Query.Pair ("Content-Length", (Integer.to_query f)));
           Util.option_map v.content_language
             (fun f  -> Query.Pair ("Content-Language", (String.to_query f)));
           Util.option_map v.content_encoding
             (fun f  -> Query.Pair ("Content-Encoding", (String.to_query f)));
           Util.option_map v.content_disposition
             (fun f  ->
                Query.Pair ("Content-Disposition", (String.to_query f)));
           Util.option_map v.cache_control
             (fun f  -> Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.body
             (fun f  -> Query.Pair ("Body", (Blob.to_query f)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.website_redirect_location
             (fun f  -> ("website_redirect_location", (String.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.metadata
             (fun f  -> ("metadata", (Metadata.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.grant_write_a_c_p
             (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.expires
             (fun f  -> ("expires", (DateTime.to_json f)));
           Util.option_map v.content_type
             (fun f  -> ("content_type", (String.to_json f)));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Util.option_map v.content_length
             (fun f  -> ("content_length", (Integer.to_json f)));
           Util.option_map v.content_language
             (fun f  -> ("content_language", (String.to_json f)));
           Util.option_map v.content_encoding
             (fun f  -> ("content_encoding", (String.to_json f)));
           Util.option_map v.content_disposition
             (fun f  -> ("content_disposition", (String.to_json f)));
           Util.option_map v.cache_control
             (fun f  -> ("cache_control", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.body (fun f  -> ("body", (Blob.to_json f)));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (ObjectCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") ObjectCannedACL.of_json);
        body = (Util.option_map (Json.lookup j "body") Blob.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        cache_control =
          (Util.option_map (Json.lookup j "cache_control") String.of_json);
        content_disposition =
          (Util.option_map (Json.lookup j "content_disposition")
             String.of_json);
        content_encoding =
          (Util.option_map (Json.lookup j "content_encoding") String.of_json);
        content_language =
          (Util.option_map (Json.lookup j "content_language") String.of_json);
        content_length =
          (Util.option_map (Json.lookup j "content_length") Integer.of_json);
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        content_type =
          (Util.option_map (Json.lookup j "content_type") String.of_json);
        expires =
          (Util.option_map (Json.lookup j "expires") DateTime.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        metadata =
          (Util.option_map (Json.lookup j "metadata") Metadata.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        website_redirect_location =
          (Util.option_map (Json.lookup j "website_redirect_location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module PutBucketAclRequest =
  struct
    type t =
      {
      a_c_l: BucketCannedACL.t option ;
      access_control_policy: AccessControlPolicy.t option ;
      bucket: String.t ;
      content_m_d5: String.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option }
    let make ?a_c_l  ?access_control_policy  ~bucket  ?content_m_d5 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write 
      ?grant_write_a_c_p  () =
      {
        a_c_l;
        access_control_policy;
        bucket;
        content_m_d5;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               BucketCannedACL.parse);
          access_control_policy =
            (Util.option_bind (Xml.member "AccessControlPolicy" xml)
               AccessControlPolicy.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Util.option_bind (Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.grant_write_a_c_p
              (fun f  ->
                 Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_write
             (fun f  -> Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.access_control_policy
             (fun f  ->
                Query.Pair
                  ("AccessControlPolicy", (AccessControlPolicy.to_query f)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (BucketCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.grant_write_a_c_p
              (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_write
             (fun f  -> ("grant_write", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.access_control_policy
             (fun f  ->
                ("access_control_policy", (AccessControlPolicy.to_json f)));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (BucketCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") BucketCannedACL.of_json);
        access_control_policy =
          (Util.option_map (Json.lookup j "access_control_policy")
             AccessControlPolicy.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write =
          (Util.option_map (Json.lookup j "grant_write") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json)
      } 
  end
module GetObjectAclRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?version_id  ?request_payer  () =
      { bucket; key; version_id; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          version_id =
            (Util.option_bind (Xml.member "versionId" xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("versionId", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Some ("key", (String.to_json v.key));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module GetBucketAclOutput =
  struct
    type t = {
      owner: Owner.t option ;
      grants: Grants.t }
    let make ?owner  ?(grants= [])  () = { owner; grants } 
    let parse xml =
      Some
        {
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse);
          grants =
            (Util.of_option []
               (Util.option_bind (Xml.member "AccessControlList" xml)
                  Grants.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("AccessControlList", (Grants.to_query v.grants)));
           Util.option_map v.owner
             (fun f  -> Query.Pair ("Owner", (Owner.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grants", (Grants.to_json v.grants));
           Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)))])
      
    let of_json j =
      {
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json);
        grants =
          (Grants.of_json (Util.of_option_exn (Json.lookup j "grants")))
      } 
  end
module GetObjectAclOutput =
  struct
    type t =
      {
      owner: Owner.t option ;
      grants: Grants.t ;
      request_charged: RequestCharged.t option }
    let make ?owner  ?(grants= [])  ?request_charged  () =
      { owner; grants; request_charged } 
    let parse xml =
      Some
        {
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse);
          grants =
            (Util.of_option []
               (Util.option_bind (Xml.member "AccessControlList" xml)
                  Grants.parse));
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Some
             (Query.Pair ("AccessControlList", (Grants.to_query v.grants)));
           Util.option_map v.owner
             (fun f  -> Query.Pair ("Owner", (Owner.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Some ("grants", (Grants.to_json v.grants));
           Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)))])
      
    let of_json j =
      {
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json);
        grants =
          (Grants.of_json (Util.of_option_exn (Json.lookup j "grants")));
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module GetBucketRequestPaymentRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module DeleteObjectsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delete: Delete.t ;
      m_f_a: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~delete  ?m_f_a  ?request_payer  () =
      { bucket; delete; m_f_a; request_payer } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          delete =
            (Xml.required "Delete"
               (Util.option_bind (Xml.member "Delete" xml) Delete.parse));
          m_f_a =
            (Util.option_bind (Xml.member "x-amz-mfa" xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.m_f_a
             (fun f  -> Query.Pair ("x-amz-mfa", (String.to_query f)));
           Some (Query.Pair ("Delete", (Delete.to_query v.delete)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.m_f_a (fun f  -> ("m_f_a", (String.to_json f)));
           Some ("delete", (Delete.to_json v.delete));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        delete =
          (Delete.of_json (Util.of_option_exn (Json.lookup j "delete")));
        m_f_a = (Util.option_map (Json.lookup j "m_f_a") String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module UploadPartOutput =
  struct
    type t =
      {
      server_side_encryption: ServerSideEncryption.t option ;
      e_tag: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?server_side_encryption  ?e_tag  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?request_charged  () =
      {
        server_side_encryption;
        e_tag;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)))])
      
    let of_json j =
      {
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module ListPartsOutput =
  struct
    type t =
      {
      bucket: String.t option ;
      key: String.t option ;
      upload_id: String.t option ;
      part_number_marker: Integer.t option ;
      next_part_number_marker: Integer.t option ;
      max_parts: Integer.t option ;
      is_truncated: Boolean.t option ;
      parts: Parts.t ;
      initiator: Initiator.t option ;
      owner: Owner.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option }
    let make ?bucket  ?key  ?upload_id  ?part_number_marker 
      ?next_part_number_marker  ?max_parts  ?is_truncated  ?(parts= []) 
      ?initiator  ?owner  ?storage_class  ?request_charged  () =
      {
        bucket;
        key;
        upload_id;
        part_number_marker;
        next_part_number_marker;
        max_parts;
        is_truncated;
        parts;
        initiator;
        owner;
        storage_class;
        request_charged
      } 
    let parse xml =
      Some
        {
          bucket = (Util.option_bind (Xml.member "Bucket" xml) String.parse);
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          upload_id =
            (Util.option_bind (Xml.member "UploadId" xml) String.parse);
          part_number_marker =
            (Util.option_bind (Xml.member "PartNumberMarker" xml)
               Integer.parse);
          next_part_number_marker =
            (Util.option_bind (Xml.member "NextPartNumberMarker" xml)
               Integer.parse);
          max_parts =
            (Util.option_bind (Xml.member "MaxParts" xml) Integer.parse);
          is_truncated =
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse);
          parts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Part" xml) Parts.parse));
          initiator =
            (Util.option_bind (Xml.member "Initiator" xml) Initiator.parse);
          owner = (Util.option_bind (Xml.member "Owner" xml) Owner.parse);
          storage_class =
            (Util.option_bind (Xml.member "StorageClass" xml)
               StorageClass.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Util.option_map v.owner
             (fun f  -> Query.Pair ("Owner", (Owner.to_query f)));
           Util.option_map v.initiator
             (fun f  -> Query.Pair ("Initiator", (Initiator.to_query f)));
           Some (Query.Pair ("Part", (Parts.to_query v.parts)));
           Util.option_map v.is_truncated
             (fun f  -> Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Util.option_map v.max_parts
             (fun f  -> Query.Pair ("MaxParts", (Integer.to_query f)));
           Util.option_map v.next_part_number_marker
             (fun f  ->
                Query.Pair ("NextPartNumberMarker", (Integer.to_query f)));
           Util.option_map v.part_number_marker
             (fun f  -> Query.Pair ("PartNumberMarker", (Integer.to_query f)));
           Util.option_map v.upload_id
             (fun f  -> Query.Pair ("UploadId", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)));
           Util.option_map v.bucket
             (fun f  -> Query.Pair ("Bucket", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.owner (fun f  -> ("owner", (Owner.to_json f)));
           Util.option_map v.initiator
             (fun f  -> ("initiator", (Initiator.to_json f)));
           Some ("parts", (Parts.to_json v.parts));
           Util.option_map v.is_truncated
             (fun f  -> ("is_truncated", (Boolean.to_json f)));
           Util.option_map v.max_parts
             (fun f  -> ("max_parts", (Integer.to_json f)));
           Util.option_map v.next_part_number_marker
             (fun f  -> ("next_part_number_marker", (Integer.to_json f)));
           Util.option_map v.part_number_marker
             (fun f  -> ("part_number_marker", (Integer.to_json f)));
           Util.option_map v.upload_id
             (fun f  -> ("upload_id", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)));
           Util.option_map v.bucket
             (fun f  -> ("bucket", (String.to_json f)))])
      
    let of_json j =
      {
        bucket = (Util.option_map (Json.lookup j "bucket") String.of_json);
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        upload_id =
          (Util.option_map (Json.lookup j "upload_id") String.of_json);
        part_number_marker =
          (Util.option_map (Json.lookup j "part_number_marker")
             Integer.of_json);
        next_part_number_marker =
          (Util.option_map (Json.lookup j "next_part_number_marker")
             Integer.of_json);
        max_parts =
          (Util.option_map (Json.lookup j "max_parts") Integer.of_json);
        is_truncated =
          (Util.option_map (Json.lookup j "is_truncated") Boolean.of_json);
        parts = (Parts.of_json (Util.of_option_exn (Json.lookup j "parts")));
        initiator =
          (Util.option_map (Json.lookup j "initiator") Initiator.of_json);
        owner = (Util.option_map (Json.lookup j "owner") Owner.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module UploadPartCopyRequest =
  struct
    type t =
      {
      bucket: String.t ;
      copy_source: String.t ;
      copy_source_if_match: String.t option ;
      copy_source_if_modified_since: DateTime.t option ;
      copy_source_if_none_match: String.t option ;
      copy_source_if_unmodified_since: DateTime.t option ;
      copy_source_range: String.t option ;
      key: String.t ;
      part_number: Integer.t ;
      upload_id: String.t ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      copy_source_s_s_e_customer_algorithm: String.t option ;
      copy_source_s_s_e_customer_key: String.t option ;
      copy_source_s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~copy_source  ?copy_source_if_match 
      ?copy_source_if_modified_since  ?copy_source_if_none_match 
      ?copy_source_if_unmodified_since  ?copy_source_range  ~key 
      ~part_number  ~upload_id  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5 
      ?copy_source_s_s_e_customer_algorithm  ?copy_source_s_s_e_customer_key 
      ?copy_source_s_s_e_customer_key_m_d5  ?request_payer  () =
      {
        bucket;
        copy_source;
        copy_source_if_match;
        copy_source_if_modified_since;
        copy_source_if_none_match;
        copy_source_if_unmodified_since;
        copy_source_range;
        key;
        part_number;
        upload_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        copy_source_s_s_e_customer_algorithm;
        copy_source_s_s_e_customer_key;
        copy_source_s_s_e_customer_key_m_d5;
        request_payer
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          copy_source =
            (Xml.required "x-amz-copy-source"
               (Util.option_bind (Xml.member "x-amz-copy-source" xml)
                  String.parse));
          copy_source_if_match =
            (Util.option_bind (Xml.member "x-amz-copy-source-if-match" xml)
               String.parse);
          copy_source_if_modified_since =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-modified-since" xml)
               DateTime.parse);
          copy_source_if_none_match =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-none-match" xml)
               String.parse);
          copy_source_if_unmodified_since =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-unmodified-since" xml)
               DateTime.parse);
          copy_source_range =
            (Util.option_bind (Xml.member "x-amz-copy-source-range" xml)
               String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          part_number =
            (Xml.required "partNumber"
               (Util.option_bind (Xml.member "partNumber" xml) Integer.parse));
          upload_id =
            (Xml.required "uploadId"
               (Util.option_bind (Xml.member "uploadId" xml) String.parse));
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          copy_source_s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          copy_source_s_s_e_customer_key =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key" xml)
               String.parse);
          copy_source_s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Some (Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some (Query.Pair ("partNumber", (Integer.to_query v.part_number)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.copy_source_range
             (fun f  ->
                Query.Pair ("x-amz-copy-source-range", (String.to_query f)));
           Util.option_map v.copy_source_if_unmodified_since
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-unmodified-since",
                    (DateTime.to_query f)));
           Util.option_map v.copy_source_if_none_match
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-none-match", (String.to_query f)));
           Util.option_map v.copy_source_if_modified_since
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-modified-since",
                    (DateTime.to_query f)));
           Util.option_map v.copy_source_if_match
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-match", (String.to_query f)));
           Some
             (Query.Pair
                ("x-amz-copy-source", (String.to_query v.copy_source)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f  ->
                ("copy_source_s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_key
             (fun f  ->
                ("copy_source_s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f  ->
                ("copy_source_s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Some ("upload_id", (String.to_json v.upload_id));
           Some ("part_number", (Integer.to_json v.part_number));
           Some ("key", (String.to_json v.key));
           Util.option_map v.copy_source_range
             (fun f  -> ("copy_source_range", (String.to_json f)));
           Util.option_map v.copy_source_if_unmodified_since
             (fun f  ->
                ("copy_source_if_unmodified_since", (DateTime.to_json f)));
           Util.option_map v.copy_source_if_none_match
             (fun f  -> ("copy_source_if_none_match", (String.to_json f)));
           Util.option_map v.copy_source_if_modified_since
             (fun f  ->
                ("copy_source_if_modified_since", (DateTime.to_json f)));
           Util.option_map v.copy_source_if_match
             (fun f  -> ("copy_source_if_match", (String.to_json f)));
           Some ("copy_source", (String.to_json v.copy_source));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        copy_source =
          (String.of_json (Util.of_option_exn (Json.lookup j "copy_source")));
        copy_source_if_match =
          (Util.option_map (Json.lookup j "copy_source_if_match")
             String.of_json);
        copy_source_if_modified_since =
          (Util.option_map (Json.lookup j "copy_source_if_modified_since")
             DateTime.of_json);
        copy_source_if_none_match =
          (Util.option_map (Json.lookup j "copy_source_if_none_match")
             String.of_json);
        copy_source_if_unmodified_since =
          (Util.option_map (Json.lookup j "copy_source_if_unmodified_since")
             DateTime.of_json);
        copy_source_range =
          (Util.option_map (Json.lookup j "copy_source_range") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        part_number =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "part_number")));
        upload_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "upload_id")));
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        copy_source_s_s_e_customer_algorithm =
          (Util.option_map
             (Json.lookup j "copy_source_s_s_e_customer_algorithm")
             String.of_json);
        copy_source_s_s_e_customer_key =
          (Util.option_map (Json.lookup j "copy_source_s_s_e_customer_key")
             String.of_json);
        copy_source_s_s_e_customer_key_m_d5 =
          (Util.option_map
             (Json.lookup j "copy_source_s_s_e_customer_key_m_d5")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module ListMultipartUploadsOutput =
  struct
    type t =
      {
      bucket: String.t option ;
      key_marker: String.t option ;
      upload_id_marker: String.t option ;
      next_key_marker: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      next_upload_id_marker: String.t option ;
      max_uploads: Integer.t option ;
      is_truncated: Boolean.t option ;
      uploads: MultipartUploadList.t ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option }
    let make ?bucket  ?key_marker  ?upload_id_marker  ?next_key_marker 
      ?prefix  ?delimiter  ?next_upload_id_marker  ?max_uploads 
      ?is_truncated  ?(uploads= [])  ?(common_prefixes= [])  ?encoding_type 
      () =
      {
        bucket;
        key_marker;
        upload_id_marker;
        next_key_marker;
        prefix;
        delimiter;
        next_upload_id_marker;
        max_uploads;
        is_truncated;
        uploads;
        common_prefixes;
        encoding_type
      } 
    let parse xml =
      Some
        {
          bucket = (Util.option_bind (Xml.member "Bucket" xml) String.parse);
          key_marker =
            (Util.option_bind (Xml.member "KeyMarker" xml) String.parse);
          upload_id_marker =
            (Util.option_bind (Xml.member "UploadIdMarker" xml) String.parse);
          next_key_marker =
            (Util.option_bind (Xml.member "NextKeyMarker" xml) String.parse);
          prefix = (Util.option_bind (Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Util.option_bind (Xml.member "Delimiter" xml) String.parse);
          next_upload_id_marker =
            (Util.option_bind (Xml.member "NextUploadIdMarker" xml)
               String.parse);
          max_uploads =
            (Util.option_bind (Xml.member "MaxUploads" xml) Integer.parse);
          is_truncated =
            (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse);
          uploads =
            (Util.of_option []
               (Util.option_bind (Xml.member "Upload" xml)
                  MultipartUploadList.parse));
          common_prefixes =
            (Util.of_option []
               (Util.option_bind (Xml.member "CommonPrefixes" xml)
                  CommonPrefixList.parse));
          encoding_type =
            (Util.option_bind (Xml.member "EncodingType" xml)
               EncodingType.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  ->
                 Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Some
             (Query.Pair ("Upload", (MultipartUploadList.to_query v.uploads)));
           Util.option_map v.is_truncated
             (fun f  -> Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Util.option_map v.max_uploads
             (fun f  -> Query.Pair ("MaxUploads", (Integer.to_query f)));
           Util.option_map v.next_upload_id_marker
             (fun f  ->
                Query.Pair ("NextUploadIdMarker", (String.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("Delimiter", (String.to_query f)));
           Util.option_map v.prefix
             (fun f  -> Query.Pair ("Prefix", (String.to_query f)));
           Util.option_map v.next_key_marker
             (fun f  -> Query.Pair ("NextKeyMarker", (String.to_query f)));
           Util.option_map v.upload_id_marker
             (fun f  -> Query.Pair ("UploadIdMarker", (String.to_query f)));
           Util.option_map v.key_marker
             (fun f  -> Query.Pair ("KeyMarker", (String.to_query f)));
           Util.option_map v.bucket
             (fun f  -> Query.Pair ("Bucket", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.encoding_type
              (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Some
             ("common_prefixes",
               (CommonPrefixList.to_json v.common_prefixes));
           Some ("uploads", (MultipartUploadList.to_json v.uploads));
           Util.option_map v.is_truncated
             (fun f  -> ("is_truncated", (Boolean.to_json f)));
           Util.option_map v.max_uploads
             (fun f  -> ("max_uploads", (Integer.to_json f)));
           Util.option_map v.next_upload_id_marker
             (fun f  -> ("next_upload_id_marker", (String.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Util.option_map v.prefix
             (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.next_key_marker
             (fun f  -> ("next_key_marker", (String.to_json f)));
           Util.option_map v.upload_id_marker
             (fun f  -> ("upload_id_marker", (String.to_json f)));
           Util.option_map v.key_marker
             (fun f  -> ("key_marker", (String.to_json f)));
           Util.option_map v.bucket
             (fun f  -> ("bucket", (String.to_json f)))])
      
    let of_json j =
      {
        bucket = (Util.option_map (Json.lookup j "bucket") String.of_json);
        key_marker =
          (Util.option_map (Json.lookup j "key_marker") String.of_json);
        upload_id_marker =
          (Util.option_map (Json.lookup j "upload_id_marker") String.of_json);
        next_key_marker =
          (Util.option_map (Json.lookup j "next_key_marker") String.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json);
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        next_upload_id_marker =
          (Util.option_map (Json.lookup j "next_upload_id_marker")
             String.of_json);
        max_uploads =
          (Util.option_map (Json.lookup j "max_uploads") Integer.of_json);
        is_truncated =
          (Util.option_map (Json.lookup j "is_truncated") Boolean.of_json);
        uploads =
          (MultipartUploadList.of_json
             (Util.of_option_exn (Json.lookup j "uploads")));
        common_prefixes =
          (CommonPrefixList.of_json
             (Util.of_option_exn (Json.lookup j "common_prefixes")));
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json)
      } 
  end
module GetBucketLoggingOutput =
  struct
    type t = {
      logging_enabled: LoggingEnabled.t option }
    let make ?logging_enabled  () = { logging_enabled } 
    let parse xml =
      Some
        {
          logging_enabled =
            (Util.option_bind (Xml.member "LoggingEnabled" xml)
               LoggingEnabled.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.logging_enabled
              (fun f  ->
                 Query.Pair ("LoggingEnabled", (LoggingEnabled.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.logging_enabled
              (fun f  -> ("logging_enabled", (LoggingEnabled.to_json f)))])
      
    let of_json j =
      {
        logging_enabled =
          (Util.option_map (Json.lookup j "logging_enabled")
             LoggingEnabled.of_json)
      } 
  end
module ListObjectVersionsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      key_marker: String.t option ;
      max_keys: Integer.t option ;
      prefix: String.t option ;
      version_id_marker: String.t option }
    let make ~bucket  ?delimiter  ?encoding_type  ?key_marker  ?max_keys 
      ?prefix  ?version_id_marker  () =
      {
        bucket;
        delimiter;
        encoding_type;
        key_marker;
        max_keys;
        prefix;
        version_id_marker
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          delimiter =
            (Util.option_bind (Xml.member "delimiter" xml) String.parse);
          encoding_type =
            (Util.option_bind (Xml.member "encoding-type" xml)
               EncodingType.parse);
          key_marker =
            (Util.option_bind (Xml.member "key-marker" xml) String.parse);
          max_keys =
            (Util.option_bind (Xml.member "max-keys" xml) Integer.parse);
          prefix = (Util.option_bind (Xml.member "prefix" xml) String.parse);
          version_id_marker =
            (Util.option_bind (Xml.member "version-id-marker" xml)
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version_id_marker
              (fun f  ->
                 Query.Pair ("version-id-marker", (String.to_query f)));
           Util.option_map v.prefix
             (fun f  -> Query.Pair ("prefix", (String.to_query f)));
           Util.option_map v.max_keys
             (fun f  -> Query.Pair ("max-keys", (Integer.to_query f)));
           Util.option_map v.key_marker
             (fun f  -> Query.Pair ("key-marker", (String.to_query f)));
           Util.option_map v.encoding_type
             (fun f  ->
                Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("delimiter", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version_id_marker
              (fun f  -> ("version_id_marker", (String.to_json f)));
           Util.option_map v.prefix
             (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.max_keys
             (fun f  -> ("max_keys", (Integer.to_json f)));
           Util.option_map v.key_marker
             (fun f  -> ("key_marker", (String.to_json f)));
           Util.option_map v.encoding_type
             (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json);
        key_marker =
          (Util.option_map (Json.lookup j "key_marker") String.of_json);
        max_keys =
          (Util.option_map (Json.lookup j "max_keys") Integer.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json);
        version_id_marker =
          (Util.option_map (Json.lookup j "version_id_marker") String.of_json)
      } 
  end
module NoSuchUpload =
  struct
    type t = unit
    let make () = () 
    let parse xml = Some () 
    let to_query v = Query.List (Util.list_filter_opt []) 
    let to_json v = `Assoc (Util.list_filter_opt []) 
    let of_json j = () 
  end
module GetBucketVersioningOutput =
  struct
    type t =
      {
      status: BucketVersioningStatus.t option ;
      m_f_a_delete: MFADeleteStatus.t option }
    let make ?status  ?m_f_a_delete  () = { status; m_f_a_delete } 
    let parse xml =
      Some
        {
          status =
            (Util.option_bind (Xml.member "Status" xml)
               BucketVersioningStatus.parse);
          m_f_a_delete =
            (Util.option_bind (Xml.member "MfaDelete" xml)
               MFADeleteStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.m_f_a_delete
              (fun f  ->
                 Query.Pair ("MfaDelete", (MFADeleteStatus.to_query f)));
           Util.option_map v.status
             (fun f  ->
                Query.Pair ("Status", (BucketVersioningStatus.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.m_f_a_delete
              (fun f  -> ("m_f_a_delete", (MFADeleteStatus.to_json f)));
           Util.option_map v.status
             (fun f  -> ("status", (BucketVersioningStatus.to_json f)))])
      
    let of_json j =
      {
        status =
          (Util.option_map (Json.lookup j "status")
             BucketVersioningStatus.of_json);
        m_f_a_delete =
          (Util.option_map (Json.lookup j "m_f_a_delete")
             MFADeleteStatus.of_json)
      } 
  end
module CopyObjectRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      copy_source: String.t ;
      copy_source_if_match: String.t option ;
      copy_source_if_modified_since: DateTime.t option ;
      copy_source_if_none_match: String.t option ;
      copy_source_if_unmodified_since: DateTime.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      metadata_directive: MetadataDirective.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      copy_source_s_s_e_customer_algorithm: String.t option ;
      copy_source_s_s_e_customer_key: String.t option ;
      copy_source_s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option }
    let make ?a_c_l  ~bucket  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_type  ~copy_source 
      ?copy_source_if_match  ?copy_source_if_modified_since 
      ?copy_source_if_none_match  ?copy_source_if_unmodified_since  ?expires 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write_a_c_p
       ~key  ?metadata  ?metadata_directive  ?server_side_encryption 
      ?storage_class  ?website_redirect_location  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?copy_source_s_s_e_customer_algorithm  ?copy_source_s_s_e_customer_key 
      ?copy_source_s_s_e_customer_key_m_d5  ?request_payer  () =
      {
        a_c_l;
        bucket;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        copy_source;
        copy_source_if_match;
        copy_source_if_modified_since;
        copy_source_if_none_match;
        copy_source_if_unmodified_since;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        metadata_directive;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        copy_source_s_s_e_customer_algorithm;
        copy_source_s_s_e_customer_key;
        copy_source_s_s_e_customer_key_m_d5;
        request_payer
      } 
    let parse xml =
      Some
        {
          a_c_l =
            (Util.option_bind (Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          cache_control =
            (Util.option_bind (Xml.member "Cache-Control" xml) String.parse);
          content_disposition =
            (Util.option_bind (Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Util.option_bind (Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Util.option_bind (Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Util.option_bind (Xml.member "Content-Type" xml) String.parse);
          copy_source =
            (Xml.required "x-amz-copy-source"
               (Util.option_bind (Xml.member "x-amz-copy-source" xml)
                  String.parse));
          copy_source_if_match =
            (Util.option_bind (Xml.member "x-amz-copy-source-if-match" xml)
               String.parse);
          copy_source_if_modified_since =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-modified-since" xml)
               DateTime.parse);
          copy_source_if_none_match =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-none-match" xml)
               String.parse);
          copy_source_if_unmodified_since =
            (Util.option_bind
               (Xml.member "x-amz-copy-source-if-unmodified-since" xml)
               DateTime.parse);
          expires =
            (Util.option_bind (Xml.member "Expires" xml) DateTime.parse);
          grant_full_control =
            (Util.option_bind (Xml.member "x-amz-grant-full-control" xml)
               String.parse);
          grant_read =
            (Util.option_bind (Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Util.option_bind (Xml.member "x-amz-grant-write-acp" xml)
               String.parse);
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          metadata =
            (Util.option_bind (Xml.member "x-amz-meta-" xml) Metadata.parse);
          metadata_directive =
            (Util.option_bind (Xml.member "x-amz-metadata-directive" xml)
               MetadataDirective.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Util.option_bind (Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Util.option_bind
               (Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          copy_source_s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          copy_source_s_s_e_customer_key =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key" xml)
               String.parse);
          copy_source_s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Util.option_bind (Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.website_redirect_location
             (fun f  ->
                Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("x-amz-storage-class", (StorageClass.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.metadata_directive
             (fun f  ->
                Query.Pair
                  ("x-amz-metadata-directive",
                    (MetadataDirective.to_query f)));
           Util.option_map v.metadata
             (fun f  -> Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)));
           Util.option_map v.grant_write_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  ->
                Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Util.option_map v.grant_read
             (fun f  -> Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Util.option_map v.grant_full_control
             (fun f  ->
                Query.Pair ("x-amz-grant-full-control", (String.to_query f)));
           Util.option_map v.expires
             (fun f  -> Query.Pair ("Expires", (DateTime.to_query f)));
           Util.option_map v.copy_source_if_unmodified_since
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-unmodified-since",
                    (DateTime.to_query f)));
           Util.option_map v.copy_source_if_none_match
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-none-match", (String.to_query f)));
           Util.option_map v.copy_source_if_modified_since
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-modified-since",
                    (DateTime.to_query f)));
           Util.option_map v.copy_source_if_match
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-if-match", (String.to_query f)));
           Some
             (Query.Pair
                ("x-amz-copy-source", (String.to_query v.copy_source)));
           Util.option_map v.content_type
             (fun f  -> Query.Pair ("Content-Type", (String.to_query f)));
           Util.option_map v.content_language
             (fun f  -> Query.Pair ("Content-Language", (String.to_query f)));
           Util.option_map v.content_encoding
             (fun f  -> Query.Pair ("Content-Encoding", (String.to_query f)));
           Util.option_map v.content_disposition
             (fun f  ->
                Query.Pair ("Content-Disposition", (String.to_query f)));
           Util.option_map v.cache_control
             (fun f  -> Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)));
           Util.option_map v.a_c_l
             (fun f  ->
                Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_payer
              (fun f  -> ("request_payer", (RequestPayer.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f  ->
                ("copy_source_s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_key
             (fun f  ->
                ("copy_source_s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f  ->
                ("copy_source_s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key
             (fun f  -> ("s_s_e_customer_key", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.website_redirect_location
             (fun f  -> ("website_redirect_location", (String.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.metadata_directive
             (fun f  -> ("metadata_directive", (MetadataDirective.to_json f)));
           Util.option_map v.metadata
             (fun f  -> ("metadata", (Metadata.to_json f)));
           Some ("key", (String.to_json v.key));
           Util.option_map v.grant_write_a_c_p
             (fun f  -> ("grant_write_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read_a_c_p
             (fun f  -> ("grant_read_a_c_p", (String.to_json f)));
           Util.option_map v.grant_read
             (fun f  -> ("grant_read", (String.to_json f)));
           Util.option_map v.grant_full_control
             (fun f  -> ("grant_full_control", (String.to_json f)));
           Util.option_map v.expires
             (fun f  -> ("expires", (DateTime.to_json f)));
           Util.option_map v.copy_source_if_unmodified_since
             (fun f  ->
                ("copy_source_if_unmodified_since", (DateTime.to_json f)));
           Util.option_map v.copy_source_if_none_match
             (fun f  -> ("copy_source_if_none_match", (String.to_json f)));
           Util.option_map v.copy_source_if_modified_since
             (fun f  ->
                ("copy_source_if_modified_since", (DateTime.to_json f)));
           Util.option_map v.copy_source_if_match
             (fun f  -> ("copy_source_if_match", (String.to_json f)));
           Some ("copy_source", (String.to_json v.copy_source));
           Util.option_map v.content_type
             (fun f  -> ("content_type", (String.to_json f)));
           Util.option_map v.content_language
             (fun f  -> ("content_language", (String.to_json f)));
           Util.option_map v.content_encoding
             (fun f  -> ("content_encoding", (String.to_json f)));
           Util.option_map v.content_disposition
             (fun f  -> ("content_disposition", (String.to_json f)));
           Util.option_map v.cache_control
             (fun f  -> ("cache_control", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket));
           Util.option_map v.a_c_l
             (fun f  -> ("a_c_l", (ObjectCannedACL.to_json f)))])
      
    let of_json j =
      {
        a_c_l =
          (Util.option_map (Json.lookup j "a_c_l") ObjectCannedACL.of_json);
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        cache_control =
          (Util.option_map (Json.lookup j "cache_control") String.of_json);
        content_disposition =
          (Util.option_map (Json.lookup j "content_disposition")
             String.of_json);
        content_encoding =
          (Util.option_map (Json.lookup j "content_encoding") String.of_json);
        content_language =
          (Util.option_map (Json.lookup j "content_language") String.of_json);
        content_type =
          (Util.option_map (Json.lookup j "content_type") String.of_json);
        copy_source =
          (String.of_json (Util.of_option_exn (Json.lookup j "copy_source")));
        copy_source_if_match =
          (Util.option_map (Json.lookup j "copy_source_if_match")
             String.of_json);
        copy_source_if_modified_since =
          (Util.option_map (Json.lookup j "copy_source_if_modified_since")
             DateTime.of_json);
        copy_source_if_none_match =
          (Util.option_map (Json.lookup j "copy_source_if_none_match")
             String.of_json);
        copy_source_if_unmodified_since =
          (Util.option_map (Json.lookup j "copy_source_if_unmodified_since")
             DateTime.of_json);
        expires =
          (Util.option_map (Json.lookup j "expires") DateTime.of_json);
        grant_full_control =
          (Util.option_map (Json.lookup j "grant_full_control")
             String.of_json);
        grant_read =
          (Util.option_map (Json.lookup j "grant_read") String.of_json);
        grant_read_a_c_p =
          (Util.option_map (Json.lookup j "grant_read_a_c_p") String.of_json);
        grant_write_a_c_p =
          (Util.option_map (Json.lookup j "grant_write_a_c_p") String.of_json);
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        metadata =
          (Util.option_map (Json.lookup j "metadata") Metadata.of_json);
        metadata_directive =
          (Util.option_map (Json.lookup j "metadata_directive")
             MetadataDirective.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        website_redirect_location =
          (Util.option_map (Json.lookup j "website_redirect_location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Util.option_map (Json.lookup j "s_s_e_customer_key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        copy_source_s_s_e_customer_algorithm =
          (Util.option_map
             (Json.lookup j "copy_source_s_s_e_customer_algorithm")
             String.of_json);
        copy_source_s_s_e_customer_key =
          (Util.option_map (Json.lookup j "copy_source_s_s_e_customer_key")
             String.of_json);
        copy_source_s_s_e_customer_key_m_d5 =
          (Util.option_map
             (Json.lookup j "copy_source_s_s_e_customer_key_m_d5")
             String.of_json);
        request_payer =
          (Util.option_map (Json.lookup j "request_payer")
             RequestPayer.of_json)
      } 
  end
module DeleteBucketCorsRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module CopyObjectOutput =
  struct
    type t =
      {
      copy_object_result: CopyObjectResult.t option ;
      expiration: String.t option ;
      copy_source_version_id: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?copy_object_result  ?expiration  ?copy_source_version_id 
      ?server_side_encryption  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?request_charged  () =
      {
        copy_object_result;
        expiration;
        copy_source_version_id;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        request_charged
      } 
    let parse xml =
      Some
        {
          copy_object_result =
            (Util.option_bind (Xml.member "CopyObjectResult" xml)
               CopyObjectResult.parse);
          expiration =
            (Util.option_bind (Xml.member "x-amz-expiration" xml)
               String.parse);
          copy_source_version_id =
            (Util.option_bind (Xml.member "x-amz-copy-source-version-id" xml)
               String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  ->
                 Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.copy_source_version_id
             (fun f  ->
                Query.Pair
                  ("x-amz-copy-source-version-id", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  -> Query.Pair ("x-amz-expiration", (String.to_query f)));
           Util.option_map v.copy_object_result
             (fun f  ->
                Query.Pair
                  ("CopyObjectResult", (CopyObjectResult.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.request_charged
              (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.copy_source_version_id
             (fun f  -> ("copy_source_version_id", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (String.to_json f)));
           Util.option_map v.copy_object_result
             (fun f  -> ("copy_object_result", (CopyObjectResult.to_json f)))])
      
    let of_json j =
      {
        copy_object_result =
          (Util.option_map (Json.lookup j "copy_object_result")
             CopyObjectResult.of_json);
        expiration =
          (Util.option_map (Json.lookup j "expiration") String.of_json);
        copy_source_version_id =
          (Util.option_map (Json.lookup j "copy_source_version_id")
             String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json)
      } 
  end
module PutBucketTaggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      tagging: Tagging.t }
    let make ~bucket  ?content_m_d5  ~tagging  () =
      { bucket; content_m_d5; tagging } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          tagging =
            (Xml.required "Tagging"
               (Util.option_bind (Xml.member "Tagging" xml) Tagging.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tagging", (Tagging.to_query v.tagging)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tagging", (Tagging.to_json v.tagging));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        tagging =
          (Tagging.of_json (Util.of_option_exn (Json.lookup j "tagging")))
      } 
  end
module DeleteBucketWebsiteRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module PutBucketWebsiteRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      website_configuration: WebsiteConfiguration.t }
    let make ~bucket  ?content_m_d5  ~website_configuration  () =
      { bucket; content_m_d5; website_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse);
          website_configuration =
            (Xml.required "WebsiteConfiguration"
               (Util.option_bind (Xml.member "WebsiteConfiguration" xml)
                  WebsiteConfiguration.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("WebsiteConfiguration",
                   (WebsiteConfiguration.to_query v.website_configuration)));
           Util.option_map v.content_m_d5
             (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("website_configuration",
                (WebsiteConfiguration.to_json v.website_configuration));
           Util.option_map v.content_m_d5
             (fun f  -> ("content_m_d5", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json);
        website_configuration =
          (WebsiteConfiguration.of_json
             (Util.of_option_exn (Json.lookup j "website_configuration")))
      } 
  end
module GetBucketLifecycleRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetBucketWebsiteRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module ListMultipartUploadsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      key_marker: String.t option ;
      max_uploads: Integer.t option ;
      prefix: String.t option ;
      upload_id_marker: String.t option }
    let make ~bucket  ?delimiter  ?encoding_type  ?key_marker  ?max_uploads 
      ?prefix  ?upload_id_marker  () =
      {
        bucket;
        delimiter;
        encoding_type;
        key_marker;
        max_uploads;
        prefix;
        upload_id_marker
      } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          delimiter =
            (Util.option_bind (Xml.member "delimiter" xml) String.parse);
          encoding_type =
            (Util.option_bind (Xml.member "encoding-type" xml)
               EncodingType.parse);
          key_marker =
            (Util.option_bind (Xml.member "key-marker" xml) String.parse);
          max_uploads =
            (Util.option_bind (Xml.member "max-uploads" xml) Integer.parse);
          prefix = (Util.option_bind (Xml.member "prefix" xml) String.parse);
          upload_id_marker =
            (Util.option_bind (Xml.member "upload-id-marker" xml)
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.upload_id_marker
              (fun f  -> Query.Pair ("upload-id-marker", (String.to_query f)));
           Util.option_map v.prefix
             (fun f  -> Query.Pair ("prefix", (String.to_query f)));
           Util.option_map v.max_uploads
             (fun f  -> Query.Pair ("max-uploads", (Integer.to_query f)));
           Util.option_map v.key_marker
             (fun f  -> Query.Pair ("key-marker", (String.to_query f)));
           Util.option_map v.encoding_type
             (fun f  ->
                Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Util.option_map v.delimiter
             (fun f  -> Query.Pair ("delimiter", (String.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.upload_id_marker
              (fun f  -> ("upload_id_marker", (String.to_json f)));
           Util.option_map v.prefix
             (fun f  -> ("prefix", (String.to_json f)));
           Util.option_map v.max_uploads
             (fun f  -> ("max_uploads", (Integer.to_json f)));
           Util.option_map v.key_marker
             (fun f  -> ("key_marker", (String.to_json f)));
           Util.option_map v.encoding_type
             (fun f  -> ("encoding_type", (EncodingType.to_json f)));
           Util.option_map v.delimiter
             (fun f  -> ("delimiter", (String.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        delimiter =
          (Util.option_map (Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Util.option_map (Json.lookup j "encoding_type")
             EncodingType.of_json);
        key_marker =
          (Util.option_map (Json.lookup j "key_marker") String.of_json);
        max_uploads =
          (Util.option_map (Json.lookup j "max_uploads") Integer.of_json);
        prefix = (Util.option_map (Json.lookup j "prefix") String.of_json);
        upload_id_marker =
          (Util.option_map (Json.lookup j "upload_id_marker") String.of_json)
      } 
  end
module GetBucketNotificationConfigurationRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetObjectOutput =
  struct
    type t =
      {
      body: Blob.t option ;
      delete_marker: Boolean.t option ;
      accept_ranges: String.t option ;
      expiration: String.t option ;
      restore: String.t option ;
      last_modified: DateTime.t option ;
      content_length: Integer.t option ;
      e_tag: String.t option ;
      missing_meta: Integer.t option ;
      version_id: String.t option ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_range: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      website_redirect_location: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      metadata: Metadata.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option ;
      replication_status: ReplicationStatus.t option }
    let make ?body  ?delete_marker  ?accept_ranges  ?expiration  ?restore 
      ?last_modified  ?content_length  ?e_tag  ?missing_meta  ?version_id 
      ?cache_control  ?content_disposition  ?content_encoding 
      ?content_language  ?content_range  ?content_type  ?expires 
      ?website_redirect_location  ?server_side_encryption  ?metadata 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?storage_class  ?request_charged 
      ?replication_status  () =
      {
        body;
        delete_marker;
        accept_ranges;
        expiration;
        restore;
        last_modified;
        content_length;
        e_tag;
        missing_meta;
        version_id;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_range;
        content_type;
        expires;
        website_redirect_location;
        server_side_encryption;
        metadata;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        storage_class;
        request_charged;
        replication_status
      } 
    let parse xml =
      Some
        {
          body = (Util.option_bind (Xml.member "Body" xml) Blob.parse);
          delete_marker =
            (Util.option_bind (Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          accept_ranges =
            (Util.option_bind (Xml.member "accept-ranges" xml) String.parse);
          expiration =
            (Util.option_bind (Xml.member "x-amz-expiration" xml)
               String.parse);
          restore =
            (Util.option_bind (Xml.member "x-amz-restore" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "Last-Modified" xml) DateTime.parse);
          content_length =
            (Util.option_bind (Xml.member "Content-Length" xml) Integer.parse);
          e_tag = (Util.option_bind (Xml.member "ETag" xml) String.parse);
          missing_meta =
            (Util.option_bind (Xml.member "x-amz-missing-meta" xml)
               Integer.parse);
          version_id =
            (Util.option_bind (Xml.member "x-amz-version-id" xml)
               String.parse);
          cache_control =
            (Util.option_bind (Xml.member "Cache-Control" xml) String.parse);
          content_disposition =
            (Util.option_bind (Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Util.option_bind (Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Util.option_bind (Xml.member "Content-Language" xml)
               String.parse);
          content_range =
            (Util.option_bind (Xml.member "Content-Range" xml) String.parse);
          content_type =
            (Util.option_bind (Xml.member "Content-Type" xml) String.parse);
          expires =
            (Util.option_bind (Xml.member "Expires" xml) DateTime.parse);
          website_redirect_location =
            (Util.option_bind
               (Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          server_side_encryption =
            (Util.option_bind (Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          metadata =
            (Util.option_bind (Xml.member "x-amz-meta-" xml) Metadata.parse);
          s_s_e_customer_algorithm =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Util.option_bind
               (Xml.member "x-amz-server-side-encryption-aws-kms-key-id" xml)
               String.parse);
          storage_class =
            (Util.option_bind (Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          request_charged =
            (Util.option_bind (Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          replication_status =
            (Util.option_bind (Xml.member "x-amz-replication-status" xml)
               ReplicationStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_status
              (fun f  ->
                 Query.Pair
                   ("x-amz-replication-status",
                     (ReplicationStatus.to_query f)));
           Util.option_map v.request_charged
             (fun f  ->
                Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Util.option_map v.storage_class
             (fun f  ->
                Query.Pair ("x-amz-storage-class", (StorageClass.to_query f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Util.option_map v.metadata
             (fun f  -> Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Util.option_map v.website_redirect_location
             (fun f  ->
                Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Util.option_map v.expires
             (fun f  -> Query.Pair ("Expires", (DateTime.to_query f)));
           Util.option_map v.content_type
             (fun f  -> Query.Pair ("Content-Type", (String.to_query f)));
           Util.option_map v.content_range
             (fun f  -> Query.Pair ("Content-Range", (String.to_query f)));
           Util.option_map v.content_language
             (fun f  -> Query.Pair ("Content-Language", (String.to_query f)));
           Util.option_map v.content_encoding
             (fun f  -> Query.Pair ("Content-Encoding", (String.to_query f)));
           Util.option_map v.content_disposition
             (fun f  ->
                Query.Pair ("Content-Disposition", (String.to_query f)));
           Util.option_map v.cache_control
             (fun f  -> Query.Pair ("Cache-Control", (String.to_query f)));
           Util.option_map v.version_id
             (fun f  -> Query.Pair ("x-amz-version-id", (String.to_query f)));
           Util.option_map v.missing_meta
             (fun f  ->
                Query.Pair ("x-amz-missing-meta", (Integer.to_query f)));
           Util.option_map v.e_tag
             (fun f  -> Query.Pair ("ETag", (String.to_query f)));
           Util.option_map v.content_length
             (fun f  -> Query.Pair ("Content-Length", (Integer.to_query f)));
           Util.option_map v.last_modified
             (fun f  -> Query.Pair ("Last-Modified", (DateTime.to_query f)));
           Util.option_map v.restore
             (fun f  -> Query.Pair ("x-amz-restore", (String.to_query f)));
           Util.option_map v.expiration
             (fun f  -> Query.Pair ("x-amz-expiration", (String.to_query f)));
           Util.option_map v.accept_ranges
             (fun f  -> Query.Pair ("accept-ranges", (String.to_query f)));
           Util.option_map v.delete_marker
             (fun f  ->
                Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)));
           Util.option_map v.body
             (fun f  -> Query.Pair ("Body", (Blob.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_status
              (fun f  ->
                 ("replication_status", (ReplicationStatus.to_json f)));
           Util.option_map v.request_charged
             (fun f  -> ("request_charged", (RequestCharged.to_json f)));
           Util.option_map v.storage_class
             (fun f  -> ("storage_class", (StorageClass.to_json f)));
           Util.option_map v.s_s_e_k_m_s_key_id
             (fun f  -> ("s_s_e_k_m_s_key_id", (String.to_json f)));
           Util.option_map v.s_s_e_customer_key_m_d5
             (fun f  -> ("s_s_e_customer_key_m_d5", (String.to_json f)));
           Util.option_map v.s_s_e_customer_algorithm
             (fun f  -> ("s_s_e_customer_algorithm", (String.to_json f)));
           Util.option_map v.metadata
             (fun f  -> ("metadata", (Metadata.to_json f)));
           Util.option_map v.server_side_encryption
             (fun f  ->
                ("server_side_encryption", (ServerSideEncryption.to_json f)));
           Util.option_map v.website_redirect_location
             (fun f  -> ("website_redirect_location", (String.to_json f)));
           Util.option_map v.expires
             (fun f  -> ("expires", (DateTime.to_json f)));
           Util.option_map v.content_type
             (fun f  -> ("content_type", (String.to_json f)));
           Util.option_map v.content_range
             (fun f  -> ("content_range", (String.to_json f)));
           Util.option_map v.content_language
             (fun f  -> ("content_language", (String.to_json f)));
           Util.option_map v.content_encoding
             (fun f  -> ("content_encoding", (String.to_json f)));
           Util.option_map v.content_disposition
             (fun f  -> ("content_disposition", (String.to_json f)));
           Util.option_map v.cache_control
             (fun f  -> ("cache_control", (String.to_json f)));
           Util.option_map v.version_id
             (fun f  -> ("version_id", (String.to_json f)));
           Util.option_map v.missing_meta
             (fun f  -> ("missing_meta", (Integer.to_json f)));
           Util.option_map v.e_tag (fun f  -> ("e_tag", (String.to_json f)));
           Util.option_map v.content_length
             (fun f  -> ("content_length", (Integer.to_json f)));
           Util.option_map v.last_modified
             (fun f  -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.restore
             (fun f  -> ("restore", (String.to_json f)));
           Util.option_map v.expiration
             (fun f  -> ("expiration", (String.to_json f)));
           Util.option_map v.accept_ranges
             (fun f  -> ("accept_ranges", (String.to_json f)));
           Util.option_map v.delete_marker
             (fun f  -> ("delete_marker", (Boolean.to_json f)));
           Util.option_map v.body (fun f  -> ("body", (Blob.to_json f)))])
      
    let of_json j =
      {
        body = (Util.option_map (Json.lookup j "body") Blob.of_json);
        delete_marker =
          (Util.option_map (Json.lookup j "delete_marker") Boolean.of_json);
        accept_ranges =
          (Util.option_map (Json.lookup j "accept_ranges") String.of_json);
        expiration =
          (Util.option_map (Json.lookup j "expiration") String.of_json);
        restore = (Util.option_map (Json.lookup j "restore") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        content_length =
          (Util.option_map (Json.lookup j "content_length") Integer.of_json);
        e_tag = (Util.option_map (Json.lookup j "e_tag") String.of_json);
        missing_meta =
          (Util.option_map (Json.lookup j "missing_meta") Integer.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        cache_control =
          (Util.option_map (Json.lookup j "cache_control") String.of_json);
        content_disposition =
          (Util.option_map (Json.lookup j "content_disposition")
             String.of_json);
        content_encoding =
          (Util.option_map (Json.lookup j "content_encoding") String.of_json);
        content_language =
          (Util.option_map (Json.lookup j "content_language") String.of_json);
        content_range =
          (Util.option_map (Json.lookup j "content_range") String.of_json);
        content_type =
          (Util.option_map (Json.lookup j "content_type") String.of_json);
        expires =
          (Util.option_map (Json.lookup j "expires") DateTime.of_json);
        website_redirect_location =
          (Util.option_map (Json.lookup j "website_redirect_location")
             String.of_json);
        server_side_encryption =
          (Util.option_map (Json.lookup j "server_side_encryption")
             ServerSideEncryption.of_json);
        metadata =
          (Util.option_map (Json.lookup j "metadata") Metadata.of_json);
        s_s_e_customer_algorithm =
          (Util.option_map (Json.lookup j "s_s_e_customer_algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Util.option_map (Json.lookup j "s_s_e_customer_key_m_d5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Util.option_map (Json.lookup j "s_s_e_k_m_s_key_id")
             String.of_json);
        storage_class =
          (Util.option_map (Json.lookup j "storage_class")
             StorageClass.of_json);
        request_charged =
          (Util.option_map (Json.lookup j "request_charged")
             RequestCharged.of_json);
        replication_status =
          (Util.option_map (Json.lookup j "replication_status")
             ReplicationStatus.of_json)
      } 
  end
module PutBucketNotificationConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      notification_configuration: NotificationConfiguration.t }
    let make ~bucket  ~notification_configuration  () =
      { bucket; notification_configuration } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          notification_configuration =
            (Xml.required "NotificationConfiguration"
               (Util.option_bind (Xml.member "NotificationConfiguration" xml)
                  NotificationConfiguration.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NotificationConfiguration",
                   (NotificationConfiguration.to_query
                      v.notification_configuration)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("notification_configuration",
                (NotificationConfiguration.to_json
                   v.notification_configuration));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        notification_configuration =
          (NotificationConfiguration.of_json
             (Util.of_option_exn (Json.lookup j "notification_configuration")))
      } 
  end
module GetBucketCorsRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module CreateBucketOutput =
  struct
    type t = {
      location: String.t option }
    let make ?location  () = { location } 
    let parse xml =
      Some
        {
          location =
            (Util.option_bind (Xml.member "Location" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.location
              (fun f  -> Query.Pair ("Location", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.location
              (fun f  -> ("location", (String.to_json f)))])
      
    let of_json j =
      {
        location =
          (Util.option_map (Json.lookup j "location") String.of_json)
      } 
  end
module PutBucketCorsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      c_o_r_s_configuration: CORSConfiguration.t option ;
      content_m_d5: String.t option }
    let make ~bucket  ?c_o_r_s_configuration  ?content_m_d5  () =
      { bucket; c_o_r_s_configuration; content_m_d5 } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse));
          c_o_r_s_configuration =
            (Util.option_bind (Xml.member "CORSConfiguration" xml)
               CORSConfiguration.parse);
          content_m_d5 =
            (Util.option_bind (Xml.member "Content-MD5" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.content_m_d5
              (fun f  -> Query.Pair ("Content-MD5", (String.to_query f)));
           Util.option_map v.c_o_r_s_configuration
             (fun f  ->
                Query.Pair
                  ("CORSConfiguration", (CORSConfiguration.to_query f)));
           Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.content_m_d5
              (fun f  -> ("content_m_d5", (String.to_json f)));
           Util.option_map v.c_o_r_s_configuration
             (fun f  ->
                ("c_o_r_s_configuration", (CORSConfiguration.to_json f)));
           Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")));
        c_o_r_s_configuration =
          (Util.option_map (Json.lookup j "c_o_r_s_configuration")
             CORSConfiguration.of_json);
        content_m_d5 =
          (Util.option_map (Json.lookup j "content_m_d5") String.of_json)
      } 
  end
module DeleteBucketLifecycleRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetBucketAclRequest =
  struct
    type t = {
      bucket: String.t }
    let make ~bucket  () = { bucket } 
    let parse xml =
      Some
        {
          bucket =
            (Xml.required "Bucket"
               (Util.option_bind (Xml.member "Bucket" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Bucket", (String.to_query v.bucket)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("bucket", (String.to_json v.bucket))])
      
    let of_json j =
      {
        bucket =
          (String.of_json (Util.of_option_exn (Json.lookup j "bucket")))
      } 
  end
module GetBucketRequestPaymentOutput =
  struct
    type t = {
      payer: Payer.t option }
    let make ?payer  () = { payer } 
    let parse xml =
      Some
        { payer = (Util.option_bind (Xml.member "Payer" xml) Payer.parse) }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.payer
              (fun f  -> Query.Pair ("Payer", (Payer.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.payer (fun f  -> ("payer", (Payer.to_json f)))])
      
    let of_json j =
      { payer = (Util.option_map (Json.lookup j "payer") Payer.of_json) } 
  end