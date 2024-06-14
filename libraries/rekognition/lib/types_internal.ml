open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module EmotionName =
  struct
    type t =
      | HAPPY 
      | SAD 
      | ANGRY 
      | CONFUSED 
      | DISGUSTED 
      | SURPRISED 
      | CALM 
      | UNKNOWN 
    let str_to_t =
      [("UNKNOWN", UNKNOWN);
      ("CALM", CALM);
      ("SURPRISED", SURPRISED);
      ("DISGUSTED", DISGUSTED);
      ("CONFUSED", CONFUSED);
      ("ANGRY", ANGRY);
      ("SAD", SAD);
      ("HAPPY", HAPPY)]
    let t_to_str =
      [(UNKNOWN, "UNKNOWN");
      (CALM, "CALM");
      (SURPRISED, "SURPRISED");
      (DISGUSTED, "DISGUSTED");
      (CONFUSED, "CONFUSED");
      (ANGRY, "ANGRY");
      (SAD, "SAD");
      (HAPPY, "HAPPY")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module LandmarkType =
  struct
    type t =
      | EyeLeft 
      | EyeRight 
      | Nose 
      | MouthLeft 
      | MouthRight 
      | LeftEyeBrowLeft 
      | LeftEyeBrowRight 
      | LeftEyeBrowUp 
      | RightEyeBrowLeft 
      | RightEyeBrowRight 
      | RightEyeBrowUp 
      | LeftEyeLeft 
      | LeftEyeRight 
      | LeftEyeUp 
      | LeftEyeDown 
      | RightEyeLeft 
      | RightEyeRight 
      | RightEyeUp 
      | RightEyeDown 
      | NoseLeft 
      | NoseRight 
      | MouthUp 
      | MouthDown 
      | LeftPupil 
      | RightPupil 
      | UpperJawlineLeft 
      | MidJawlineLeft 
      | ChinBottom 
      | MidJawlineRight 
      | UpperJawlineRight 
    let str_to_t =
      [("upperJawlineRight", UpperJawlineRight);
      ("midJawlineRight", MidJawlineRight);
      ("chinBottom", ChinBottom);
      ("midJawlineLeft", MidJawlineLeft);
      ("upperJawlineLeft", UpperJawlineLeft);
      ("rightPupil", RightPupil);
      ("leftPupil", LeftPupil);
      ("mouthDown", MouthDown);
      ("mouthUp", MouthUp);
      ("noseRight", NoseRight);
      ("noseLeft", NoseLeft);
      ("rightEyeDown", RightEyeDown);
      ("rightEyeUp", RightEyeUp);
      ("rightEyeRight", RightEyeRight);
      ("rightEyeLeft", RightEyeLeft);
      ("leftEyeDown", LeftEyeDown);
      ("leftEyeUp", LeftEyeUp);
      ("leftEyeRight", LeftEyeRight);
      ("leftEyeLeft", LeftEyeLeft);
      ("rightEyeBrowUp", RightEyeBrowUp);
      ("rightEyeBrowRight", RightEyeBrowRight);
      ("rightEyeBrowLeft", RightEyeBrowLeft);
      ("leftEyeBrowUp", LeftEyeBrowUp);
      ("leftEyeBrowRight", LeftEyeBrowRight);
      ("leftEyeBrowLeft", LeftEyeBrowLeft);
      ("mouthRight", MouthRight);
      ("mouthLeft", MouthLeft);
      ("nose", Nose);
      ("eyeRight", EyeRight);
      ("eyeLeft", EyeLeft)]
    let t_to_str =
      [(UpperJawlineRight, "upperJawlineRight");
      (MidJawlineRight, "midJawlineRight");
      (ChinBottom, "chinBottom");
      (MidJawlineLeft, "midJawlineLeft");
      (UpperJawlineLeft, "upperJawlineLeft");
      (RightPupil, "rightPupil");
      (LeftPupil, "leftPupil");
      (MouthDown, "mouthDown");
      (MouthUp, "mouthUp");
      (NoseRight, "noseRight");
      (NoseLeft, "noseLeft");
      (RightEyeDown, "rightEyeDown");
      (RightEyeUp, "rightEyeUp");
      (RightEyeRight, "rightEyeRight");
      (RightEyeLeft, "rightEyeLeft");
      (LeftEyeDown, "leftEyeDown");
      (LeftEyeUp, "leftEyeUp");
      (LeftEyeRight, "leftEyeRight");
      (LeftEyeLeft, "leftEyeLeft");
      (RightEyeBrowUp, "rightEyeBrowUp");
      (RightEyeBrowRight, "rightEyeBrowRight");
      (RightEyeBrowLeft, "rightEyeBrowLeft");
      (LeftEyeBrowUp, "leftEyeBrowUp");
      (LeftEyeBrowRight, "leftEyeBrowRight");
      (LeftEyeBrowLeft, "leftEyeBrowLeft");
      (MouthRight, "mouthRight");
      (MouthLeft, "mouthLeft");
      (Nose, "nose");
      (EyeRight, "eyeRight");
      (EyeLeft, "eyeLeft")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Emotion =
  struct
    type t = {
      type_: EmotionName.t option ;
      confidence: Float.t option }
    let make ?type_  ?confidence  () = { type_; confidence }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) EmotionName.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (EmotionName.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (EmotionName.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") EmotionName.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module GenderType =
  struct
    type t =
      | Male 
      | Female 
    let str_to_t = [("Female", Female); ("Male", Male)]
    let t_to_str = [(Female, "Female"); (Male, "Male")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Landmark =
  struct
    type t =
      {
      type_: LandmarkType.t option ;
      x: Float.t option ;
      y: Float.t option }
    let make ?type_  ?x  ?y  () = { type_; x; y }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) LandmarkType.parse);
          x = (Util.option_bind (Xml.member "X" xml) Float.parse);
          y = (Util.option_bind (Xml.member "Y" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.y
              (fun f -> Query.Pair ("Y", (Float.to_query f)));
           Util.option_map v.x
             (fun f -> Query.Pair ("X", (Float.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (LandmarkType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.y (fun f -> ("y", (Float.to_json f)));
           Util.option_map v.x (fun f -> ("x", (Float.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (LandmarkType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") LandmarkType.of_json);
        x = (Util.option_map (Json.lookup j "x") Float.of_json);
        y = (Util.option_map (Json.lookup j "y") Float.of_json)
      }
  end
module BoundingBox =
  struct
    type t =
      {
      width: Float.t option ;
      height: Float.t option ;
      left: Float.t option ;
      top: Float.t option }
    let make ?width  ?height  ?left  ?top  () = { width; height; left; top }
    let parse xml =
      Some
        {
          width = (Util.option_bind (Xml.member "Width" xml) Float.parse);
          height = (Util.option_bind (Xml.member "Height" xml) Float.parse);
          left = (Util.option_bind (Xml.member "Left" xml) Float.parse);
          top = (Util.option_bind (Xml.member "Top" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.top
              (fun f -> Query.Pair ("Top", (Float.to_query f)));
           Util.option_map v.left
             (fun f -> Query.Pair ("Left", (Float.to_query f)));
           Util.option_map v.height
             (fun f -> Query.Pair ("Height", (Float.to_query f)));
           Util.option_map v.width
             (fun f -> Query.Pair ("Width", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.top (fun f -> ("top", (Float.to_json f)));
           Util.option_map v.left (fun f -> ("left", (Float.to_json f)));
           Util.option_map v.height (fun f -> ("height", (Float.to_json f)));
           Util.option_map v.width (fun f -> ("width", (Float.to_json f)))])
    let of_json j =
      {
        width = (Util.option_map (Json.lookup j "width") Float.of_json);
        height = (Util.option_map (Json.lookup j "height") Float.of_json);
        left = (Util.option_map (Json.lookup j "left") Float.of_json);
        top = (Util.option_map (Json.lookup j "top") Float.of_json)
      }
  end
module AgeRange =
  struct
    type t = {
      low: Integer.t option ;
      high: Integer.t option }
    let make ?low  ?high  () = { low; high }
    let parse xml =
      Some
        {
          low = (Util.option_bind (Xml.member "Low" xml) Integer.parse);
          high = (Util.option_bind (Xml.member "High" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.high
              (fun f -> Query.Pair ("High", (Integer.to_query f)));
           Util.option_map v.low
             (fun f -> Query.Pair ("Low", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.high (fun f -> ("high", (Integer.to_json f)));
           Util.option_map v.low (fun f -> ("low", (Integer.to_json f)))])
    let of_json j =
      {
        low = (Util.option_map (Json.lookup j "low") Integer.of_json);
        high = (Util.option_map (Json.lookup j "high") Integer.of_json)
      }
  end
module Beard =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Emotions =
  struct
    type t = Emotion.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Emotion.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Emotion.to_query v
    let to_json v = `List (List.map Emotion.to_json v)
    let of_json j = Json.to_list Emotion.of_json j
  end
module EyeOpen =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Eyeglasses =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Gender =
  struct
    type t = {
      value: GenderType.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value =
            (Util.option_bind (Xml.member "Value" xml) GenderType.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (GenderType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value
             (fun f -> ("value", (GenderType.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") GenderType.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module ImageQuality =
  struct
    type t = {
      brightness: Float.t option ;
      sharpness: Float.t option }
    let make ?brightness  ?sharpness  () = { brightness; sharpness }
    let parse xml =
      Some
        {
          brightness =
            (Util.option_bind (Xml.member "Brightness" xml) Float.parse);
          sharpness =
            (Util.option_bind (Xml.member "Sharpness" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sharpness
              (fun f -> Query.Pair ("Sharpness", (Float.to_query f)));
           Util.option_map v.brightness
             (fun f -> Query.Pair ("Brightness", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sharpness
              (fun f -> ("sharpness", (Float.to_json f)));
           Util.option_map v.brightness
             (fun f -> ("brightness", (Float.to_json f)))])
    let of_json j =
      {
        brightness =
          (Util.option_map (Json.lookup j "brightness") Float.of_json);
        sharpness =
          (Util.option_map (Json.lookup j "sharpness") Float.of_json)
      }
  end
module Landmarks =
  struct
    type t = Landmark.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Landmark.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Landmark.to_query v
    let to_json v = `List (List.map Landmark.to_json v)
    let of_json j = Json.to_list Landmark.of_json j
  end
module MouthOpen =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Mustache =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Pose =
  struct
    type t =
      {
      roll: Float.t option ;
      yaw: Float.t option ;
      pitch: Float.t option }
    let make ?roll  ?yaw  ?pitch  () = { roll; yaw; pitch }
    let parse xml =
      Some
        {
          roll = (Util.option_bind (Xml.member "Roll" xml) Float.parse);
          yaw = (Util.option_bind (Xml.member "Yaw" xml) Float.parse);
          pitch = (Util.option_bind (Xml.member "Pitch" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.pitch
              (fun f -> Query.Pair ("Pitch", (Float.to_query f)));
           Util.option_map v.yaw
             (fun f -> Query.Pair ("Yaw", (Float.to_query f)));
           Util.option_map v.roll
             (fun f -> Query.Pair ("Roll", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.pitch (fun f -> ("pitch", (Float.to_json f)));
           Util.option_map v.yaw (fun f -> ("yaw", (Float.to_json f)));
           Util.option_map v.roll (fun f -> ("roll", (Float.to_json f)))])
    let of_json j =
      {
        roll = (Util.option_map (Json.lookup j "roll") Float.of_json);
        yaw = (Util.option_map (Json.lookup j "yaw") Float.of_json);
        pitch = (Util.option_map (Json.lookup j "pitch") Float.of_json)
      }
  end
module Smile =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Sunglasses =
  struct
    type t = {
      value: Boolean.t option ;
      confidence: Float.t option }
    let make ?value  ?confidence  () = { value; confidence }
    let parse xml =
      Some
        {
          value = (Util.option_bind (Xml.member "Value" xml) Boolean.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Boolean.to_json f)))])
    let of_json j =
      {
        value = (Util.option_map (Json.lookup j "value") Boolean.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Instance =
  struct
    type t =
      {
      bounding_box: BoundingBox.t option ;
      confidence: Float.t option }
    let make ?bounding_box  ?confidence  () = { bounding_box; confidence }
    let parse xml =
      Some
        {
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Parent =
  struct
    type t = {
      name: String.t option }
    let make ?name  () = { name }
    let parse xml =
      Some { name = (Util.option_bind (Xml.member "Name" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name
              (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      { name = (Util.option_map (Json.lookup j "name") String.of_json) }
  end
module Point =
  struct
    type t = {
      x: Float.t option ;
      y: Float.t option }
    let make ?x  ?y  () = { x; y }
    let parse xml =
      Some
        {
          x = (Util.option_bind (Xml.member "X" xml) Float.parse);
          y = (Util.option_bind (Xml.member "Y" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.y
              (fun f -> Query.Pair ("Y", (Float.to_query f)));
           Util.option_map v.x
             (fun f -> Query.Pair ("X", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.y (fun f -> ("y", (Float.to_json f)));
           Util.option_map v.x (fun f -> ("x", (Float.to_json f)))])
    let of_json j =
      {
        x = (Util.option_map (Json.lookup j "x") Float.of_json);
        y = (Util.option_map (Json.lookup j "y") Float.of_json)
      }
  end
module Face =
  struct
    type t =
      {
      face_id: String.t option ;
      bounding_box: BoundingBox.t option ;
      image_id: String.t option ;
      external_image_id: String.t option ;
      confidence: Float.t option }
    let make ?face_id  ?bounding_box  ?image_id  ?external_image_id 
      ?confidence  () =
      { face_id; bounding_box; image_id; external_image_id; confidence }
    let parse xml =
      Some
        {
          face_id = (Util.option_bind (Xml.member "FaceId" xml) String.parse);
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          image_id =
            (Util.option_bind (Xml.member "ImageId" xml) String.parse);
          external_image_id =
            (Util.option_bind (Xml.member "ExternalImageId" xml) String.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.external_image_id
             (fun f -> Query.Pair ("ExternalImageId", (String.to_query f)));
           Util.option_map v.image_id
             (fun f -> Query.Pair ("ImageId", (String.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)));
           Util.option_map v.face_id
             (fun f -> Query.Pair ("FaceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.external_image_id
             (fun f -> ("external_image_id", (String.to_json f)));
           Util.option_map v.image_id
             (fun f -> ("image_id", (String.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)));
           Util.option_map v.face_id
             (fun f -> ("face_id", (String.to_json f)))])
    let of_json j =
      {
        face_id = (Util.option_map (Json.lookup j "face_id") String.of_json);
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        image_id =
          (Util.option_map (Json.lookup j "image_id") String.of_json);
        external_image_id =
          (Util.option_map (Json.lookup j "external_image_id") String.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module FaceDetail =
  struct
    type t =
      {
      bounding_box: BoundingBox.t option ;
      age_range: AgeRange.t option ;
      smile: Smile.t option ;
      eyeglasses: Eyeglasses.t option ;
      sunglasses: Sunglasses.t option ;
      gender: Gender.t option ;
      beard: Beard.t option ;
      mustache: Mustache.t option ;
      eyes_open: EyeOpen.t option ;
      mouth_open: MouthOpen.t option ;
      emotions: Emotions.t ;
      landmarks: Landmarks.t ;
      pose: Pose.t option ;
      quality: ImageQuality.t option ;
      confidence: Float.t option }
    let make ?bounding_box  ?age_range  ?smile  ?eyeglasses  ?sunglasses 
      ?gender  ?beard  ?mustache  ?eyes_open  ?mouth_open  ?(emotions= []) 
      ?(landmarks= [])  ?pose  ?quality  ?confidence  () =
      {
        bounding_box;
        age_range;
        smile;
        eyeglasses;
        sunglasses;
        gender;
        beard;
        mustache;
        eyes_open;
        mouth_open;
        emotions;
        landmarks;
        pose;
        quality;
        confidence
      }
    let parse xml =
      Some
        {
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          age_range =
            (Util.option_bind (Xml.member "AgeRange" xml) AgeRange.parse);
          smile = (Util.option_bind (Xml.member "Smile" xml) Smile.parse);
          eyeglasses =
            (Util.option_bind (Xml.member "Eyeglasses" xml) Eyeglasses.parse);
          sunglasses =
            (Util.option_bind (Xml.member "Sunglasses" xml) Sunglasses.parse);
          gender = (Util.option_bind (Xml.member "Gender" xml) Gender.parse);
          beard = (Util.option_bind (Xml.member "Beard" xml) Beard.parse);
          mustache =
            (Util.option_bind (Xml.member "Mustache" xml) Mustache.parse);
          eyes_open =
            (Util.option_bind (Xml.member "EyesOpen" xml) EyeOpen.parse);
          mouth_open =
            (Util.option_bind (Xml.member "MouthOpen" xml) MouthOpen.parse);
          emotions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Emotions" xml) Emotions.parse));
          landmarks =
            (Util.of_option []
               (Util.option_bind (Xml.member "Landmarks" xml) Landmarks.parse));
          pose = (Util.option_bind (Xml.member "Pose" xml) Pose.parse);
          quality =
            (Util.option_bind (Xml.member "Quality" xml) ImageQuality.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.quality
             (fun f -> Query.Pair ("Quality", (ImageQuality.to_query f)));
           Util.option_map v.pose
             (fun f -> Query.Pair ("Pose", (Pose.to_query f)));
           Some
             (Query.Pair
                ("Landmarks.member", (Landmarks.to_query v.landmarks)));
           Some
             (Query.Pair ("Emotions.member", (Emotions.to_query v.emotions)));
           Util.option_map v.mouth_open
             (fun f -> Query.Pair ("MouthOpen", (MouthOpen.to_query f)));
           Util.option_map v.eyes_open
             (fun f -> Query.Pair ("EyesOpen", (EyeOpen.to_query f)));
           Util.option_map v.mustache
             (fun f -> Query.Pair ("Mustache", (Mustache.to_query f)));
           Util.option_map v.beard
             (fun f -> Query.Pair ("Beard", (Beard.to_query f)));
           Util.option_map v.gender
             (fun f -> Query.Pair ("Gender", (Gender.to_query f)));
           Util.option_map v.sunglasses
             (fun f -> Query.Pair ("Sunglasses", (Sunglasses.to_query f)));
           Util.option_map v.eyeglasses
             (fun f -> Query.Pair ("Eyeglasses", (Eyeglasses.to_query f)));
           Util.option_map v.smile
             (fun f -> Query.Pair ("Smile", (Smile.to_query f)));
           Util.option_map v.age_range
             (fun f -> Query.Pair ("AgeRange", (AgeRange.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.quality
             (fun f -> ("quality", (ImageQuality.to_json f)));
           Util.option_map v.pose (fun f -> ("pose", (Pose.to_json f)));
           Some ("landmarks", (Landmarks.to_json v.landmarks));
           Some ("emotions", (Emotions.to_json v.emotions));
           Util.option_map v.mouth_open
             (fun f -> ("mouth_open", (MouthOpen.to_json f)));
           Util.option_map v.eyes_open
             (fun f -> ("eyes_open", (EyeOpen.to_json f)));
           Util.option_map v.mustache
             (fun f -> ("mustache", (Mustache.to_json f)));
           Util.option_map v.beard (fun f -> ("beard", (Beard.to_json f)));
           Util.option_map v.gender (fun f -> ("gender", (Gender.to_json f)));
           Util.option_map v.sunglasses
             (fun f -> ("sunglasses", (Sunglasses.to_json f)));
           Util.option_map v.eyeglasses
             (fun f -> ("eyeglasses", (Eyeglasses.to_json f)));
           Util.option_map v.smile (fun f -> ("smile", (Smile.to_json f)));
           Util.option_map v.age_range
             (fun f -> ("age_range", (AgeRange.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        age_range =
          (Util.option_map (Json.lookup j "age_range") AgeRange.of_json);
        smile = (Util.option_map (Json.lookup j "smile") Smile.of_json);
        eyeglasses =
          (Util.option_map (Json.lookup j "eyeglasses") Eyeglasses.of_json);
        sunglasses =
          (Util.option_map (Json.lookup j "sunglasses") Sunglasses.of_json);
        gender = (Util.option_map (Json.lookup j "gender") Gender.of_json);
        beard = (Util.option_map (Json.lookup j "beard") Beard.of_json);
        mustache =
          (Util.option_map (Json.lookup j "mustache") Mustache.of_json);
        eyes_open =
          (Util.option_map (Json.lookup j "eyes_open") EyeOpen.of_json);
        mouth_open =
          (Util.option_map (Json.lookup j "mouth_open") MouthOpen.of_json);
        emotions =
          (Emotions.of_json (Util.of_option_exn (Json.lookup j "emotions")));
        landmarks =
          (Landmarks.of_json (Util.of_option_exn (Json.lookup j "landmarks")));
        pose = (Util.option_map (Json.lookup j "pose") Pose.of_json);
        quality =
          (Util.option_map (Json.lookup j "quality") ImageQuality.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module Instances =
  struct
    type t = Instance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Instance.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Instance.to_query v
    let to_json v = `List (List.map Instance.to_json v)
    let of_json j = Json.to_list Instance.of_json j
  end
module Parents =
  struct
    type t = Parent.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Parent.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Parent.to_query v
    let to_json v = `List (List.map Parent.to_json v)
    let of_json j = Json.to_list Parent.of_json j
  end
module Polygon =
  struct
    type t = Point.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Point.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Point.to_query v
    let to_json v = `List (List.map Point.to_json v)
    let of_json j = Json.to_list Point.of_json j
  end
module FaceMatch =
  struct
    type t = {
      similarity: Float.t option ;
      face: Face.t option }
    let make ?similarity  ?face  () = { similarity; face }
    let parse xml =
      Some
        {
          similarity =
            (Util.option_bind (Xml.member "Similarity" xml) Float.parse);
          face = (Util.option_bind (Xml.member "Face" xml) Face.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> Query.Pair ("Face", (Face.to_query f)));
           Util.option_map v.similarity
             (fun f -> Query.Pair ("Similarity", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face (fun f -> ("face", (Face.to_json f)));
           Util.option_map v.similarity
             (fun f -> ("similarity", (Float.to_json f)))])
    let of_json j =
      {
        similarity =
          (Util.option_map (Json.lookup j "similarity") Float.of_json);
        face = (Util.option_map (Json.lookup j "face") Face.of_json)
      }
  end
module Reason =
  struct
    type t =
      | EXCEEDS_MAX_FACES 
      | EXTREME_POSE 
      | LOW_BRIGHTNESS 
      | LOW_SHARPNESS 
      | LOW_CONFIDENCE 
      | SMALL_BOUNDING_BOX 
    let str_to_t =
      [("SMALL_BOUNDING_BOX", SMALL_BOUNDING_BOX);
      ("LOW_CONFIDENCE", LOW_CONFIDENCE);
      ("LOW_SHARPNESS", LOW_SHARPNESS);
      ("LOW_BRIGHTNESS", LOW_BRIGHTNESS);
      ("EXTREME_POSE", EXTREME_POSE);
      ("EXCEEDS_MAX_FACES", EXCEEDS_MAX_FACES)]
    let t_to_str =
      [(SMALL_BOUNDING_BOX, "SMALL_BOUNDING_BOX");
      (LOW_CONFIDENCE, "LOW_CONFIDENCE");
      (LOW_SHARPNESS, "LOW_SHARPNESS");
      (LOW_BRIGHTNESS, "LOW_BRIGHTNESS");
      (EXTREME_POSE, "EXTREME_POSE");
      (EXCEEDS_MAX_FACES, "EXCEEDS_MAX_FACES")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Urls =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module PersonDetail =
  struct
    type t =
      {
      index: Long.t option ;
      bounding_box: BoundingBox.t option ;
      face: FaceDetail.t option }
    let make ?index  ?bounding_box  ?face  () = { index; bounding_box; face }
    let parse xml =
      Some
        {
          index = (Util.option_bind (Xml.member "Index" xml) Long.parse);
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          face = (Util.option_bind (Xml.member "Face" xml) FaceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> Query.Pair ("Face", (FaceDetail.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)));
           Util.option_map v.index
             (fun f -> Query.Pair ("Index", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> ("face", (FaceDetail.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)));
           Util.option_map v.index (fun f -> ("index", (Long.to_json f)))])
    let of_json j =
      {
        index = (Util.option_map (Json.lookup j "index") Long.of_json);
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        face = (Util.option_map (Json.lookup j "face") FaceDetail.of_json)
      }
  end
module ComparedFace =
  struct
    type t =
      {
      bounding_box: BoundingBox.t option ;
      confidence: Float.t option ;
      landmarks: Landmarks.t ;
      pose: Pose.t option ;
      quality: ImageQuality.t option }
    let make ?bounding_box  ?confidence  ?(landmarks= [])  ?pose  ?quality 
      () = { bounding_box; confidence; landmarks; pose; quality }
    let parse xml =
      Some
        {
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse);
          landmarks =
            (Util.of_option []
               (Util.option_bind (Xml.member "Landmarks" xml) Landmarks.parse));
          pose = (Util.option_bind (Xml.member "Pose" xml) Pose.parse);
          quality =
            (Util.option_bind (Xml.member "Quality" xml) ImageQuality.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.quality
              (fun f -> Query.Pair ("Quality", (ImageQuality.to_query f)));
           Util.option_map v.pose
             (fun f -> Query.Pair ("Pose", (Pose.to_query f)));
           Some
             (Query.Pair
                ("Landmarks.member", (Landmarks.to_query v.landmarks)));
           Util.option_map v.confidence
             (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.quality
              (fun f -> ("quality", (ImageQuality.to_json f)));
           Util.option_map v.pose (fun f -> ("pose", (Pose.to_json f)));
           Some ("landmarks", (Landmarks.to_json v.landmarks));
           Util.option_map v.confidence
             (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json);
        landmarks =
          (Landmarks.of_json (Util.of_option_exn (Json.lookup j "landmarks")));
        pose = (Util.option_map (Json.lookup j "pose") Pose.of_json);
        quality =
          (Util.option_map (Json.lookup j "quality") ImageQuality.of_json)
      }
  end
module Label =
  struct
    type t =
      {
      name: String.t option ;
      confidence: Float.t option ;
      instances: Instances.t ;
      parents: Parents.t }
    let make ?name  ?confidence  ?(instances= [])  ?(parents= [])  () =
      { name; confidence; instances; parents }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse);
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse));
          parents =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parents" xml) Parents.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Parents.member", (Parents.to_query v.parents)));
           Some
             (Query.Pair
                ("Instances.member", (Instances.to_query v.instances)));
           Util.option_map v.confidence
             (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parents", (Parents.to_json v.parents));
           Some ("instances", (Instances.to_json v.instances));
           Util.option_map v.confidence
             (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json);
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")));
        parents =
          (Parents.of_json (Util.of_option_exn (Json.lookup j "parents")))
      }
  end
module Geometry =
  struct
    type t = {
      bounding_box: BoundingBox.t option ;
      polygon: Polygon.t }
    let make ?bounding_box  ?(polygon= [])  () = { bounding_box; polygon }
    let parse xml =
      Some
        {
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          polygon =
            (Util.of_option []
               (Util.option_bind (Xml.member "Polygon" xml) Polygon.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Polygon.member", (Polygon.to_query v.polygon)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("polygon", (Polygon.to_json v.polygon));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        polygon =
          (Polygon.of_json (Util.of_option_exn (Json.lookup j "polygon")))
      }
  end
module TextTypes =
  struct
    type t =
      | LINE 
      | WORD 
    let str_to_t = [("WORD", WORD); ("LINE", LINE)]
    let t_to_str = [(WORD, "WORD"); (LINE, "LINE")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module FaceMatchList =
  struct
    type t = FaceMatch.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map FaceMatch.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list FaceMatch.to_query v
    let to_json v = `List (List.map FaceMatch.to_json v)
    let of_json j = Json.to_list FaceMatch.of_json j
  end
module ModerationLabel =
  struct
    type t =
      {
      confidence: Float.t option ;
      name: String.t option ;
      parent_name: String.t option }
    let make ?confidence  ?name  ?parent_name  () =
      { confidence; name; parent_name }
    let parse xml =
      Some
        {
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          parent_name =
            (Util.option_bind (Xml.member "ParentName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.parent_name
              (fun f -> Query.Pair ("ParentName", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.confidence
             (fun f -> Query.Pair ("Confidence", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parent_name
              (fun f -> ("parent_name", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.confidence
             (fun f -> ("confidence", (Float.to_json f)))])
    let of_json j =
      {
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        parent_name =
          (Util.option_map (Json.lookup j "parent_name") String.of_json)
      }
  end
module Reasons =
  struct
    type t = Reason.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Reason.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Reason.to_query v
    let to_json v = `List (List.map Reason.to_json v)
    let of_json j = Json.to_list Reason.of_json j
  end
module StreamProcessorStatus =
  struct
    type t =
      | STOPPED 
      | STARTING 
      | RUNNING 
      | FAILED 
      | STOPPING 
    let str_to_t =
      [("STOPPING", STOPPING);
      ("FAILED", FAILED);
      ("RUNNING", RUNNING);
      ("STARTING", STARTING);
      ("STOPPED", STOPPED)]
    let t_to_str =
      [(STOPPING, "STOPPING");
      (FAILED, "FAILED");
      (RUNNING, "RUNNING");
      (STARTING, "STARTING");
      (STOPPED, "STOPPED")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CelebrityDetail =
  struct
    type t =
      {
      urls: Urls.t ;
      name: String.t option ;
      id: String.t option ;
      confidence: Float.t option ;
      bounding_box: BoundingBox.t option ;
      face: FaceDetail.t option }
    let make ?(urls= [])  ?name  ?id  ?confidence  ?bounding_box  ?face  () =
      { urls; name; id; confidence; bounding_box; face }
    let parse xml =
      Some
        {
          urls =
            (Util.of_option []
               (Util.option_bind (Xml.member "Urls" xml) Urls.parse));
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse);
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          face = (Util.option_bind (Xml.member "Face" xml) FaceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> Query.Pair ("Face", (FaceDetail.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)));
           Util.option_map v.confidence
             (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Some (Query.Pair ("Urls.member", (Urls.to_query v.urls)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> ("face", (FaceDetail.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)));
           Util.option_map v.confidence
             (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Some ("urls", (Urls.to_json v.urls))])
    let of_json j =
      {
        urls = (Urls.of_json (Util.of_option_exn (Json.lookup j "urls")));
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json);
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        face = (Util.option_map (Json.lookup j "face") FaceDetail.of_json)
      }
  end
module S3Object =
  struct
    type t =
      {
      bucket: String.t option ;
      name: String.t option ;
      version: String.t option }
    let make ?bucket  ?name  ?version  () = { bucket; name; version }
    let parse xml =
      Some
        {
          bucket = (Util.option_bind (Xml.member "Bucket" xml) String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          version =
            (Util.option_bind (Xml.member "Version" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version
              (fun f -> Query.Pair ("Version", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.bucket
             (fun f -> Query.Pair ("Bucket", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version
              (fun f -> ("version", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.bucket (fun f -> ("bucket", (String.to_json f)))])
    let of_json j =
      {
        bucket = (Util.option_map (Json.lookup j "bucket") String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        version = (Util.option_map (Json.lookup j "version") String.of_json)
      }
  end
module PersonDetection =
  struct
    type t = {
      timestamp: Long.t option ;
      person: PersonDetail.t option }
    let make ?timestamp  ?person  () = { timestamp; person }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          person =
            (Util.option_bind (Xml.member "Person" xml) PersonDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.person
              (fun f -> Query.Pair ("Person", (PersonDetail.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.person
              (fun f -> ("person", (PersonDetail.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        person =
          (Util.option_map (Json.lookup j "person") PersonDetail.of_json)
      }
  end
module FaceDetection =
  struct
    type t = {
      timestamp: Long.t option ;
      face: FaceDetail.t option }
    let make ?timestamp  ?face  () = { timestamp; face }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          face = (Util.option_bind (Xml.member "Face" xml) FaceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> Query.Pair ("Face", (FaceDetail.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> ("face", (FaceDetail.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        face = (Util.option_map (Json.lookup j "face") FaceDetail.of_json)
      }
  end
module KinesisVideoStream =
  struct
    type t = {
      arn: String.t option }
    let make ?arn  () = { arn }
    let parse xml =
      Some { arn = (Util.option_bind (Xml.member "Arn" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.arn
              (fun f -> Query.Pair ("Arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      { arn = (Util.option_map (Json.lookup j "arn") String.of_json) }
  end
module KinesisDataStream =
  struct
    type t = {
      arn: String.t option }
    let make ?arn  () = { arn }
    let parse xml =
      Some { arn = (Util.option_bind (Xml.member "Arn" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.arn
              (fun f -> Query.Pair ("Arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      { arn = (Util.option_map (Json.lookup j "arn") String.of_json) }
  end
module FaceSearchSettings =
  struct
    type t =
      {
      collection_id: String.t option ;
      face_match_threshold: Float.t option }
    let make ?collection_id  ?face_match_threshold  () =
      { collection_id; face_match_threshold }
    let parse xml =
      Some
        {
          collection_id =
            (Util.option_bind (Xml.member "CollectionId" xml) String.parse);
          face_match_threshold =
            (Util.option_bind (Xml.member "FaceMatchThreshold" xml)
               Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> Query.Pair ("FaceMatchThreshold", (Float.to_query f)));
           Util.option_map v.collection_id
             (fun f -> Query.Pair ("CollectionId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> ("face_match_threshold", (Float.to_json f)));
           Util.option_map v.collection_id
             (fun f -> ("collection_id", (String.to_json f)))])
    let of_json j =
      {
        collection_id =
          (Util.option_map (Json.lookup j "collection_id") String.of_json);
        face_match_threshold =
          (Util.option_map (Json.lookup j "face_match_threshold")
             Float.of_json)
      }
  end
module Celebrity =
  struct
    type t =
      {
      urls: Urls.t ;
      name: String.t option ;
      id: String.t option ;
      face: ComparedFace.t option ;
      match_confidence: Float.t option }
    let make ?(urls= [])  ?name  ?id  ?face  ?match_confidence  () =
      { urls; name; id; face; match_confidence }
    let parse xml =
      Some
        {
          urls =
            (Util.of_option []
               (Util.option_bind (Xml.member "Urls" xml) Urls.parse));
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          face =
            (Util.option_bind (Xml.member "Face" xml) ComparedFace.parse);
          match_confidence =
            (Util.option_bind (Xml.member "MatchConfidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.match_confidence
              (fun f -> Query.Pair ("MatchConfidence", (Float.to_query f)));
           Util.option_map v.face
             (fun f -> Query.Pair ("Face", (ComparedFace.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Some (Query.Pair ("Urls.member", (Urls.to_query v.urls)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.match_confidence
              (fun f -> ("match_confidence", (Float.to_json f)));
           Util.option_map v.face
             (fun f -> ("face", (ComparedFace.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Some ("urls", (Urls.to_json v.urls))])
    let of_json j =
      {
        urls = (Urls.of_json (Util.of_option_exn (Json.lookup j "urls")));
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        face = (Util.option_map (Json.lookup j "face") ComparedFace.of_json);
        match_confidence =
          (Util.option_map (Json.lookup j "match_confidence") Float.of_json)
      }
  end
module CompareFacesMatch =
  struct
    type t = {
      similarity: Float.t option ;
      face: ComparedFace.t option }
    let make ?similarity  ?face  () = { similarity; face }
    let parse xml =
      Some
        {
          similarity =
            (Util.option_bind (Xml.member "Similarity" xml) Float.parse);
          face =
            (Util.option_bind (Xml.member "Face" xml) ComparedFace.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> Query.Pair ("Face", (ComparedFace.to_query f)));
           Util.option_map v.similarity
             (fun f -> Query.Pair ("Similarity", (Float.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face
              (fun f -> ("face", (ComparedFace.to_json f)));
           Util.option_map v.similarity
             (fun f -> ("similarity", (Float.to_json f)))])
    let of_json j =
      {
        similarity =
          (Util.option_map (Json.lookup j "similarity") Float.of_json);
        face = (Util.option_map (Json.lookup j "face") ComparedFace.of_json)
      }
  end
module LabelDetection =
  struct
    type t = {
      timestamp: Long.t option ;
      label: Label.t option }
    let make ?timestamp  ?label  () = { timestamp; label }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          label = (Util.option_bind (Xml.member "Label" xml) Label.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.label
              (fun f -> Query.Pair ("Label", (Label.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.label (fun f -> ("label", (Label.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        label = (Util.option_map (Json.lookup j "label") Label.of_json)
      }
  end
module Attribute =
  struct
    type t =
      | DEFAULT 
      | ALL 
    let str_to_t = [("ALL", ALL); ("DEFAULT", DEFAULT)]
    let t_to_str = [(ALL, "ALL"); (DEFAULT, "DEFAULT")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module TextDetection =
  struct
    type t =
      {
      detected_text: String.t option ;
      type_: TextTypes.t option ;
      id: Integer.t option ;
      parent_id: Integer.t option ;
      confidence: Float.t option ;
      geometry: Geometry.t option }
    let make ?detected_text  ?type_  ?id  ?parent_id  ?confidence  ?geometry 
      () = { detected_text; type_; id; parent_id; confidence; geometry }
    let parse xml =
      Some
        {
          detected_text =
            (Util.option_bind (Xml.member "DetectedText" xml) String.parse);
          type_ = (Util.option_bind (Xml.member "Type" xml) TextTypes.parse);
          id = (Util.option_bind (Xml.member "Id" xml) Integer.parse);
          parent_id =
            (Util.option_bind (Xml.member "ParentId" xml) Integer.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse);
          geometry =
            (Util.option_bind (Xml.member "Geometry" xml) Geometry.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.geometry
              (fun f -> Query.Pair ("Geometry", (Geometry.to_query f)));
           Util.option_map v.confidence
             (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.parent_id
             (fun f -> Query.Pair ("ParentId", (Integer.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (Integer.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (TextTypes.to_query f)));
           Util.option_map v.detected_text
             (fun f -> Query.Pair ("DetectedText", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.geometry
              (fun f -> ("geometry", (Geometry.to_json f)));
           Util.option_map v.confidence
             (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.parent_id
             (fun f -> ("parent_id", (Integer.to_json f)));
           Util.option_map v.id (fun f -> ("id", (Integer.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (TextTypes.to_json f)));
           Util.option_map v.detected_text
             (fun f -> ("detected_text", (String.to_json f)))])
    let of_json j =
      {
        detected_text =
          (Util.option_map (Json.lookup j "detected_text") String.of_json);
        type_ = (Util.option_map (Json.lookup j "type_") TextTypes.of_json);
        id = (Util.option_map (Json.lookup j "id") Integer.of_json);
        parent_id =
          (Util.option_map (Json.lookup j "parent_id") Integer.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json);
        geometry =
          (Util.option_map (Json.lookup j "geometry") Geometry.of_json)
      }
  end
module PersonMatch =
  struct
    type t =
      {
      timestamp: Long.t option ;
      person: PersonDetail.t option ;
      face_matches: FaceMatchList.t }
    let make ?timestamp  ?person  ?(face_matches= [])  () =
      { timestamp; person; face_matches }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          person =
            (Util.option_bind (Xml.member "Person" xml) PersonDetail.parse);
          face_matches =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceMatches" xml)
                  FaceMatchList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("FaceMatches.member",
                   (FaceMatchList.to_query v.face_matches)));
           Util.option_map v.person
             (fun f -> Query.Pair ("Person", (PersonDetail.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("face_matches", (FaceMatchList.to_json v.face_matches));
           Util.option_map v.person
             (fun f -> ("person", (PersonDetail.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        person =
          (Util.option_map (Json.lookup j "person") PersonDetail.of_json);
        face_matches =
          (FaceMatchList.of_json
             (Util.of_option_exn (Json.lookup j "face_matches")))
      }
  end
module ContentModerationDetection =
  struct
    type t =
      {
      timestamp: Long.t option ;
      moderation_label: ModerationLabel.t option }
    let make ?timestamp  ?moderation_label  () =
      { timestamp; moderation_label }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          moderation_label =
            (Util.option_bind (Xml.member "ModerationLabel" xml)
               ModerationLabel.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.moderation_label
              (fun f ->
                 Query.Pair ("ModerationLabel", (ModerationLabel.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.moderation_label
              (fun f -> ("moderation_label", (ModerationLabel.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        moderation_label =
          (Util.option_map (Json.lookup j "moderation_label")
             ModerationLabel.of_json)
      }
  end
module FaceRecord =
  struct
    type t = {
      face: Face.t option ;
      face_detail: FaceDetail.t option }
    let make ?face  ?face_detail  () = { face; face_detail }
    let parse xml =
      Some
        {
          face = (Util.option_bind (Xml.member "Face" xml) Face.parse);
          face_detail =
            (Util.option_bind (Xml.member "FaceDetail" xml) FaceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_detail
              (fun f -> Query.Pair ("FaceDetail", (FaceDetail.to_query f)));
           Util.option_map v.face
             (fun f -> Query.Pair ("Face", (Face.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_detail
              (fun f -> ("face_detail", (FaceDetail.to_json f)));
           Util.option_map v.face (fun f -> ("face", (Face.to_json f)))])
    let of_json j =
      {
        face = (Util.option_map (Json.lookup j "face") Face.of_json);
        face_detail =
          (Util.option_map (Json.lookup j "face_detail") FaceDetail.of_json)
      }
  end
module UnindexedFace =
  struct
    type t = {
      reasons: Reasons.t ;
      face_detail: FaceDetail.t option }
    let make ?(reasons= [])  ?face_detail  () = { reasons; face_detail }
    let parse xml =
      Some
        {
          reasons =
            (Util.of_option []
               (Util.option_bind (Xml.member "Reasons" xml) Reasons.parse));
          face_detail =
            (Util.option_bind (Xml.member "FaceDetail" xml) FaceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_detail
              (fun f -> Query.Pair ("FaceDetail", (FaceDetail.to_query f)));
           Some (Query.Pair ("Reasons.member", (Reasons.to_query v.reasons)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_detail
              (fun f -> ("face_detail", (FaceDetail.to_json f)));
           Some ("reasons", (Reasons.to_json v.reasons))])
    let of_json j =
      {
        reasons =
          (Reasons.of_json (Util.of_option_exn (Json.lookup j "reasons")));
        face_detail =
          (Util.option_map (Json.lookup j "face_detail") FaceDetail.of_json)
      }
  end
module StreamProcessor =
  struct
    type t = {
      name: String.t option ;
      status: StreamProcessorStatus.t option }
    let make ?name  ?status  () = { name; status }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StreamProcessorStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f ->
                 Query.Pair ("Status", (StreamProcessorStatus.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (StreamProcessorStatus.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StreamProcessorStatus.of_json)
      }
  end
module CelebrityRecognition =
  struct
    type t = {
      timestamp: Long.t option ;
      celebrity: CelebrityDetail.t option }
    let make ?timestamp  ?celebrity  () = { timestamp; celebrity }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) Long.parse);
          celebrity =
            (Util.option_bind (Xml.member "Celebrity" xml)
               CelebrityDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.celebrity
              (fun f ->
                 Query.Pair ("Celebrity", (CelebrityDetail.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.celebrity
              (fun f -> ("celebrity", (CelebrityDetail.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (Long.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") Long.of_json);
        celebrity =
          (Util.option_map (Json.lookup j "celebrity")
             CelebrityDetail.of_json)
      }
  end
module Image =
  struct
    type t = {
      bytes: Blob.t option ;
      s3_object: S3Object.t option }
    let make ?bytes  ?s3_object  () = { bytes; s3_object }
    let parse xml =
      Some
        {
          bytes = (Util.option_bind (Xml.member "Bytes" xml) Blob.parse);
          s3_object =
            (Util.option_bind (Xml.member "S3Object" xml) S3Object.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s3_object
              (fun f -> Query.Pair ("S3Object", (S3Object.to_query f)));
           Util.option_map v.bytes
             (fun f -> Query.Pair ("Bytes", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s3_object
              (fun f -> ("s3_object", (S3Object.to_json f)));
           Util.option_map v.bytes (fun f -> ("bytes", (Blob.to_json f)))])
    let of_json j =
      {
        bytes = (Util.option_map (Json.lookup j "bytes") Blob.of_json);
        s3_object =
          (Util.option_map (Json.lookup j "s3_object") S3Object.of_json)
      }
  end
module ModerationLabels =
  struct
    type t = ModerationLabel.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ModerationLabel.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ModerationLabel.to_query v
    let to_json v = `List (List.map ModerationLabel.to_json v)
    let of_json j = Json.to_list ModerationLabel.of_json j
  end
module NotificationChannel =
  struct
    type t = {
      s_n_s_topic_arn: String.t ;
      role_arn: String.t }
    let make ~s_n_s_topic_arn  ~role_arn  () = { s_n_s_topic_arn; role_arn }
    let parse xml =
      Some
        {
          s_n_s_topic_arn =
            (Xml.required "SNSTopicArn"
               (Util.option_bind (Xml.member "SNSTopicArn" xml) String.parse));
          role_arn =
            (Xml.required "RoleArn"
               (Util.option_bind (Xml.member "RoleArn" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("RoleArn", (String.to_query v.role_arn)));
           Some
             (Query.Pair ("SNSTopicArn", (String.to_query v.s_n_s_topic_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("role_arn", (String.to_json v.role_arn));
           Some ("s_n_s_topic_arn", (String.to_json v.s_n_s_topic_arn))])
    let of_json j =
      {
        s_n_s_topic_arn =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s_n_s_topic_arn")));
        role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "role_arn")))
      }
  end
module Video =
  struct
    type t = {
      s3_object: S3Object.t option }
    let make ?s3_object  () = { s3_object }
    let parse xml =
      Some
        {
          s3_object =
            (Util.option_bind (Xml.member "S3Object" xml) S3Object.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s3_object
              (fun f -> Query.Pair ("S3Object", (S3Object.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s3_object
              (fun f -> ("s3_object", (S3Object.to_json f)))])
    let of_json j =
      {
        s3_object =
          (Util.option_map (Json.lookup j "s3_object") S3Object.of_json)
      }
  end
module ContentModerationSortBy =
  struct
    type t =
      | NAME 
      | TIMESTAMP 
    let str_to_t = [("TIMESTAMP", TIMESTAMP); ("NAME", NAME)]
    let t_to_str = [(TIMESTAMP, "TIMESTAMP"); (NAME, "NAME")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module PersonDetections =
  struct
    type t = PersonDetection.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PersonDetection.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PersonDetection.to_query v
    let to_json v = `List (List.map PersonDetection.to_json v)
    let of_json j = Json.to_list PersonDetection.of_json j
  end
module VideoJobStatus =
  struct
    type t =
      | IN_PROGRESS 
      | SUCCEEDED 
      | FAILED 
    let str_to_t =
      [("FAILED", FAILED);
      ("SUCCEEDED", SUCCEEDED);
      ("IN_PROGRESS", IN_PROGRESS)]
    let t_to_str =
      [(FAILED, "FAILED");
      (SUCCEEDED, "SUCCEEDED");
      (IN_PROGRESS, "IN_PROGRESS")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module VideoMetadata =
  struct
    type t =
      {
      codec: String.t option ;
      duration_millis: Long.t option ;
      format: String.t option ;
      frame_rate: Float.t option ;
      frame_height: Long.t option ;
      frame_width: Long.t option }
    let make ?codec  ?duration_millis  ?format  ?frame_rate  ?frame_height 
      ?frame_width  () =
      { codec; duration_millis; format; frame_rate; frame_height; frame_width
      }
    let parse xml =
      Some
        {
          codec = (Util.option_bind (Xml.member "Codec" xml) String.parse);
          duration_millis =
            (Util.option_bind (Xml.member "DurationMillis" xml) Long.parse);
          format = (Util.option_bind (Xml.member "Format" xml) String.parse);
          frame_rate =
            (Util.option_bind (Xml.member "FrameRate" xml) Float.parse);
          frame_height =
            (Util.option_bind (Xml.member "FrameHeight" xml) Long.parse);
          frame_width =
            (Util.option_bind (Xml.member "FrameWidth" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.frame_width
              (fun f -> Query.Pair ("FrameWidth", (Long.to_query f)));
           Util.option_map v.frame_height
             (fun f -> Query.Pair ("FrameHeight", (Long.to_query f)));
           Util.option_map v.frame_rate
             (fun f -> Query.Pair ("FrameRate", (Float.to_query f)));
           Util.option_map v.format
             (fun f -> Query.Pair ("Format", (String.to_query f)));
           Util.option_map v.duration_millis
             (fun f -> Query.Pair ("DurationMillis", (Long.to_query f)));
           Util.option_map v.codec
             (fun f -> Query.Pair ("Codec", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.frame_width
              (fun f -> ("frame_width", (Long.to_json f)));
           Util.option_map v.frame_height
             (fun f -> ("frame_height", (Long.to_json f)));
           Util.option_map v.frame_rate
             (fun f -> ("frame_rate", (Float.to_json f)));
           Util.option_map v.format (fun f -> ("format", (String.to_json f)));
           Util.option_map v.duration_millis
             (fun f -> ("duration_millis", (Long.to_json f)));
           Util.option_map v.codec (fun f -> ("codec", (String.to_json f)))])
    let of_json j =
      {
        codec = (Util.option_map (Json.lookup j "codec") String.of_json);
        duration_millis =
          (Util.option_map (Json.lookup j "duration_millis") Long.of_json);
        format = (Util.option_map (Json.lookup j "format") String.of_json);
        frame_rate =
          (Util.option_map (Json.lookup j "frame_rate") Float.of_json);
        frame_height =
          (Util.option_map (Json.lookup j "frame_height") Long.of_json);
        frame_width =
          (Util.option_map (Json.lookup j "frame_width") Long.of_json)
      }
  end
module PersonTrackingSortBy =
  struct
    type t =
      | INDEX 
      | TIMESTAMP 
    let str_to_t = [("TIMESTAMP", TIMESTAMP); ("INDEX", INDEX)]
    let t_to_str = [(TIMESTAMP, "TIMESTAMP"); (INDEX, "INDEX")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module FaceDetections =
  struct
    type t = FaceDetection.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map FaceDetection.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list FaceDetection.to_query v
    let to_json v = `List (List.map FaceDetection.to_json v)
    let of_json j = Json.to_list FaceDetection.of_json j
  end
module StreamProcessorInput =
  struct
    type t = {
      kinesis_video_stream: KinesisVideoStream.t option }
    let make ?kinesis_video_stream  () = { kinesis_video_stream }
    let parse xml =
      Some
        {
          kinesis_video_stream =
            (Util.option_bind (Xml.member "KinesisVideoStream" xml)
               KinesisVideoStream.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.kinesis_video_stream
              (fun f ->
                 Query.Pair
                   ("KinesisVideoStream", (KinesisVideoStream.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.kinesis_video_stream
              (fun f ->
                 ("kinesis_video_stream", (KinesisVideoStream.to_json f)))])
    let of_json j =
      {
        kinesis_video_stream =
          (Util.option_map (Json.lookup j "kinesis_video_stream")
             KinesisVideoStream.of_json)
      }
  end
module StreamProcessorOutput =
  struct
    type t = {
      kinesis_data_stream: KinesisDataStream.t option }
    let make ?kinesis_data_stream  () = { kinesis_data_stream }
    let parse xml =
      Some
        {
          kinesis_data_stream =
            (Util.option_bind (Xml.member "KinesisDataStream" xml)
               KinesisDataStream.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.kinesis_data_stream
              (fun f ->
                 Query.Pair
                   ("KinesisDataStream", (KinesisDataStream.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.kinesis_data_stream
              (fun f ->
                 ("kinesis_data_stream", (KinesisDataStream.to_json f)))])
    let of_json j =
      {
        kinesis_data_stream =
          (Util.option_map (Json.lookup j "kinesis_data_stream")
             KinesisDataStream.of_json)
      }
  end
module StreamProcessorSettings =
  struct
    type t = {
      face_search: FaceSearchSettings.t option }
    let make ?face_search  () = { face_search }
    let parse xml =
      Some
        {
          face_search =
            (Util.option_bind (Xml.member "FaceSearch" xml)
               FaceSearchSettings.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_search
              (fun f ->
                 Query.Pair ("FaceSearch", (FaceSearchSettings.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_search
              (fun f -> ("face_search", (FaceSearchSettings.to_json f)))])
    let of_json j =
      {
        face_search =
          (Util.option_map (Json.lookup j "face_search")
             FaceSearchSettings.of_json)
      }
  end
module CelebrityList =
  struct
    type t = Celebrity.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Celebrity.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Celebrity.to_query v
    let to_json v = `List (List.map Celebrity.to_json v)
    let of_json j = Json.to_list Celebrity.of_json j
  end
module ComparedFaceList =
  struct
    type t = ComparedFace.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ComparedFace.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ComparedFace.to_query v
    let to_json v = `List (List.map ComparedFace.to_json v)
    let of_json j = Json.to_list ComparedFace.of_json j
  end
module OrientationCorrection =
  struct
    type t =
      | ROTATE_0 
      | ROTATE_90 
      | ROTATE_180 
      | ROTATE_270 
    let str_to_t =
      [("ROTATE_270", ROTATE_270);
      ("ROTATE_180", ROTATE_180);
      ("ROTATE_90", ROTATE_90);
      ("ROTATE_0", ROTATE_0)]
    let t_to_str =
      [(ROTATE_270, "ROTATE_270");
      (ROTATE_180, "ROTATE_180");
      (ROTATE_90, "ROTATE_90");
      (ROTATE_0, "ROTATE_0")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CompareFacesMatchList =
  struct
    type t = CompareFacesMatch.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CompareFacesMatch.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list CompareFacesMatch.to_query v
    let to_json v = `List (List.map CompareFacesMatch.to_json v)
    let of_json j = Json.to_list CompareFacesMatch.of_json j
  end
module CompareFacesUnmatchList =
  struct
    type t = ComparedFace.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ComparedFace.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ComparedFace.to_query v
    let to_json v = `List (List.map ComparedFace.to_json v)
    let of_json j = Json.to_list ComparedFace.of_json j
  end
module ComparedSourceImageFace =
  struct
    type t =
      {
      bounding_box: BoundingBox.t option ;
      confidence: Float.t option }
    let make ?bounding_box  ?confidence  () = { bounding_box; confidence }
    let parse xml =
      Some
        {
          bounding_box =
            (Util.option_bind (Xml.member "BoundingBox" xml)
               BoundingBox.parse);
          confidence =
            (Util.option_bind (Xml.member "Confidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> Query.Pair ("Confidence", (Float.to_query f)));
           Util.option_map v.bounding_box
             (fun f -> Query.Pair ("BoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.confidence
              (fun f -> ("confidence", (Float.to_json f)));
           Util.option_map v.bounding_box
             (fun f -> ("bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        bounding_box =
          (Util.option_map (Json.lookup j "bounding_box") BoundingBox.of_json);
        confidence =
          (Util.option_map (Json.lookup j "confidence") Float.of_json)
      }
  end
module LabelDetections =
  struct
    type t = LabelDetection.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LabelDetection.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list LabelDetection.to_query v
    let to_json v = `List (List.map LabelDetection.to_json v)
    let of_json j = Json.to_list LabelDetection.of_json j
  end
module FaceIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CollectionIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module FaceModelVersionList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Attributes =
  struct
    type t = Attribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Attribute.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Attribute.to_query v
    let to_json v = `List (List.map Attribute.to_json v)
    let of_json j = Json.to_list Attribute.of_json j
  end
module QualityFilter =
  struct
    type t =
      | NONE 
      | AUTO 
    let str_to_t = [("AUTO", AUTO); ("NONE", NONE)]
    let t_to_str = [(AUTO, "AUTO"); (NONE, "NONE")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Labels =
  struct
    type t = Label.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Label.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Label.to_query v
    let to_json v = `List (List.map Label.to_json v)
    let of_json j = Json.to_list Label.of_json j
  end
module TextDetectionList =
  struct
    type t = TextDetection.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TextDetection.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TextDetection.to_query v
    let to_json v = `List (List.map TextDetection.to_json v)
    let of_json j = Json.to_list TextDetection.of_json j
  end
module PersonMatches =
  struct
    type t = PersonMatch.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map PersonMatch.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PersonMatch.to_query v
    let to_json v = `List (List.map PersonMatch.to_json v)
    let of_json j = Json.to_list PersonMatch.of_json j
  end
module ContentModerationDetections =
  struct
    type t = ContentModerationDetection.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ContentModerationDetection.parse (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list ContentModerationDetection.to_query v
    let to_json v = `List (List.map ContentModerationDetection.to_json v)
    let of_json j = Json.to_list ContentModerationDetection.of_json j
  end
module FaceDetailList =
  struct
    type t = FaceDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map FaceDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list FaceDetail.to_query v
    let to_json v = `List (List.map FaceDetail.to_json v)
    let of_json j = Json.to_list FaceDetail.of_json j
  end
module FaceList =
  struct
    type t = Face.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Face.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Face.to_query v
    let to_json v = `List (List.map Face.to_json v)
    let of_json j = Json.to_list Face.of_json j
  end
module LabelDetectionSortBy =
  struct
    type t =
      | NAME 
      | TIMESTAMP 
    let str_to_t = [("TIMESTAMP", TIMESTAMP); ("NAME", NAME)]
    let t_to_str = [(TIMESTAMP, "TIMESTAMP"); (NAME, "NAME")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module FaceAttributes =
  struct
    type t =
      | DEFAULT 
      | ALL 
    let str_to_t = [("ALL", ALL); ("DEFAULT", DEFAULT)]
    let t_to_str = [(ALL, "ALL"); (DEFAULT, "DEFAULT")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module FaceRecordList =
  struct
    type t = FaceRecord.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map FaceRecord.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list FaceRecord.to_query v
    let to_json v = `List (List.map FaceRecord.to_json v)
    let of_json j = Json.to_list FaceRecord.of_json j
  end
module UnindexedFaces =
  struct
    type t = UnindexedFace.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map UnindexedFace.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list UnindexedFace.to_query v
    let to_json v = `List (List.map UnindexedFace.to_json v)
    let of_json j = Json.to_list UnindexedFace.of_json j
  end
module StreamProcessorList =
  struct
    type t = StreamProcessor.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StreamProcessor.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StreamProcessor.to_query v
    let to_json v = `List (List.map StreamProcessor.to_json v)
    let of_json j = Json.to_list StreamProcessor.of_json j
  end
module CelebrityRecognitionSortBy =
  struct
    type t =
      | ID 
      | TIMESTAMP 
    let str_to_t = [("TIMESTAMP", TIMESTAMP); ("ID", ID)]
    let t_to_str = [(TIMESTAMP, "TIMESTAMP"); (ID, "ID")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module FaceSearchSortBy =
  struct
    type t =
      | INDEX 
      | TIMESTAMP 
    let str_to_t = [("TIMESTAMP", TIMESTAMP); ("INDEX", INDEX)]
    let t_to_str = [(TIMESTAMP, "TIMESTAMP"); (INDEX, "INDEX")]
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CelebrityRecognitions =
  struct
    type t = CelebrityRecognition.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CelebrityRecognition.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list CelebrityRecognition.to_query v
    let to_json v = `List (List.map CelebrityRecognition.to_json v)
    let of_json j = Json.to_list CelebrityRecognition.of_json j
  end
module ListCollectionsRequest =
  struct
    type t = {
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ?next_token  ?max_results  () = { next_token; max_results }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module AccessDeniedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteCollectionResponse =
  struct
    type t = {
      status_code: Integer.t option }
    let make ?status_code  () = { status_code }
    let parse xml =
      Some
        {
          status_code =
            (Util.option_bind (Xml.member "StatusCode" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status_code
              (fun f -> Query.Pair ("StatusCode", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status_code
              (fun f -> ("status_code", (Integer.to_json f)))])
    let of_json j =
      {
        status_code =
          (Util.option_map (Json.lookup j "status_code") Integer.of_json)
      }
  end
module InvalidPaginationTokenException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RecognizeCelebritiesRequest =
  struct
    type t = {
      image: Image.t }
    let make ~image  () = { image }
    let parse xml =
      Some
        {
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Image", (Image.to_query v.image)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("image", (Image.to_json v.image))])
    let of_json j =
      { image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")))
      }
  end
module SearchFacesByImageRequest =
  struct
    type t =
      {
      collection_id: String.t ;
      image: Image.t ;
      max_faces: Integer.t option ;
      face_match_threshold: Float.t option }
    let make ~collection_id  ~image  ?max_faces  ?face_match_threshold  () =
      { collection_id; image; max_faces; face_match_threshold }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse));
          max_faces =
            (Util.option_bind (Xml.member "MaxFaces" xml) Integer.parse);
          face_match_threshold =
            (Util.option_bind (Xml.member "FaceMatchThreshold" xml)
               Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> Query.Pair ("FaceMatchThreshold", (Float.to_query f)));
           Util.option_map v.max_faces
             (fun f -> Query.Pair ("MaxFaces", (Integer.to_query f)));
           Some (Query.Pair ("Image", (Image.to_query v.image)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> ("face_match_threshold", (Float.to_json f)));
           Util.option_map v.max_faces
             (fun f -> ("max_faces", (Integer.to_json f)));
           Some ("image", (Image.to_json v.image));
           Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")));
        max_faces =
          (Util.option_map (Json.lookup j "max_faces") Integer.of_json);
        face_match_threshold =
          (Util.option_map (Json.lookup j "face_match_threshold")
             Float.of_json)
      }
  end
module StartStreamProcessorRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module DetectModerationLabelsResponse =
  struct
    type t =
      {
      moderation_labels: ModerationLabels.t ;
      moderation_model_version: String.t option }
    let make ?(moderation_labels= [])  ?moderation_model_version  () =
      { moderation_labels; moderation_model_version }
    let parse xml =
      Some
        {
          moderation_labels =
            (Util.of_option []
               (Util.option_bind (Xml.member "ModerationLabels" xml)
                  ModerationLabels.parse));
          moderation_model_version =
            (Util.option_bind (Xml.member "ModerationModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.moderation_model_version
              (fun f ->
                 Query.Pair ("ModerationModelVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("ModerationLabels.member",
                  (ModerationLabels.to_query v.moderation_labels)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.moderation_model_version
              (fun f -> ("moderation_model_version", (String.to_json f)));
           Some
             ("moderation_labels",
               (ModerationLabels.to_json v.moderation_labels))])
    let of_json j =
      {
        moderation_labels =
          (ModerationLabels.of_json
             (Util.of_option_exn (Json.lookup j "moderation_labels")));
        moderation_model_version =
          (Util.option_map (Json.lookup j "moderation_model_version")
             String.of_json)
      }
  end
module StartLabelDetectionRequest =
  struct
    type t =
      {
      video: Video.t ;
      client_request_token: String.t option ;
      min_confidence: Float.t option ;
      notification_channel: NotificationChannel.t option ;
      job_tag: String.t option }
    let make ~video  ?client_request_token  ?min_confidence 
      ?notification_channel  ?job_tag  () =
      {
        video;
        client_request_token;
        min_confidence;
        notification_channel;
        job_tag
      }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          min_confidence =
            (Util.option_bind (Xml.member "MinConfidence" xml) Float.parse);
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Util.option_map v.min_confidence
             (fun f -> Query.Pair ("MinConfidence", (Float.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Util.option_map v.min_confidence
             (fun f -> ("min_confidence", (Float.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        min_confidence =
          (Util.option_map (Json.lookup j "min_confidence") Float.of_json);
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module ThrottlingException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeStreamProcessorRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module CreateCollectionResponse =
  struct
    type t =
      {
      status_code: Integer.t option ;
      collection_arn: String.t option ;
      face_model_version: String.t option }
    let make ?status_code  ?collection_arn  ?face_model_version  () =
      { status_code; collection_arn; face_model_version }
    let parse xml =
      Some
        {
          status_code =
            (Util.option_bind (Xml.member "StatusCode" xml) Integer.parse);
          collection_arn =
            (Util.option_bind (Xml.member "CollectionArn" xml) String.parse);
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Util.option_map v.collection_arn
             (fun f -> Query.Pair ("CollectionArn", (String.to_query f)));
           Util.option_map v.status_code
             (fun f -> Query.Pair ("StatusCode", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> ("face_model_version", (String.to_json f)));
           Util.option_map v.collection_arn
             (fun f -> ("collection_arn", (String.to_json f)));
           Util.option_map v.status_code
             (fun f -> ("status_code", (Integer.to_json f)))])
    let of_json j =
      {
        status_code =
          (Util.option_map (Json.lookup j "status_code") Integer.of_json);
        collection_arn =
          (Util.option_map (Json.lookup j "collection_arn") String.of_json);
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json)
      }
  end
module GetContentModerationRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      sort_by: ContentModerationSortBy.t option }
    let make ~job_id  ?max_results  ?next_token  ?sort_by  () =
      { job_id; max_results; next_token; sort_by }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          sort_by =
            (Util.option_bind (Xml.member "SortBy" xml)
               ContentModerationSortBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f ->
                 Query.Pair ("SortBy", (ContentModerationSortBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> ("sort_by", (ContentModerationSortBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        sort_by =
          (Util.option_map (Json.lookup j "sort_by")
             ContentModerationSortBy.of_json)
      }
  end
module GetPersonTrackingResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      video_metadata: VideoMetadata.t option ;
      next_token: String.t option ;
      persons: PersonDetections.t }
    let make ?job_status  ?status_message  ?video_metadata  ?next_token 
      ?(persons= [])  () =
      { job_status; status_message; video_metadata; next_token; persons }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          persons =
            (Util.of_option []
               (Util.option_bind (Xml.member "Persons" xml)
                  PersonDetections.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Persons.member", (PersonDetections.to_query v.persons)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("persons", (PersonDetections.to_json v.persons));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        persons =
          (PersonDetections.of_json
             (Util.of_option_exn (Json.lookup j "persons")))
      }
  end
module InvalidParameterException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListFacesRequest =
  struct
    type t =
      {
      collection_id: String.t ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ~collection_id  ?next_token  ?max_results  () =
      { collection_id; next_token; max_results }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module CreateCollectionRequest =
  struct
    type t = {
      collection_id: String.t }
    let make ~collection_id  () = { collection_id }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")))
      }
  end
module GetPersonTrackingRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      sort_by: PersonTrackingSortBy.t option }
    let make ~job_id  ?max_results  ?next_token  ?sort_by  () =
      { job_id; max_results; next_token; sort_by }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          sort_by =
            (Util.option_bind (Xml.member "SortBy" xml)
               PersonTrackingSortBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f ->
                 Query.Pair ("SortBy", (PersonTrackingSortBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> ("sort_by", (PersonTrackingSortBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        sort_by =
          (Util.option_map (Json.lookup j "sort_by")
             PersonTrackingSortBy.of_json)
      }
  end
module StopStreamProcessorRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module GetFaceDetectionResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      video_metadata: VideoMetadata.t option ;
      next_token: String.t option ;
      faces: FaceDetections.t }
    let make ?job_status  ?status_message  ?video_metadata  ?next_token 
      ?(faces= [])  () =
      { job_status; status_message; video_metadata; next_token; faces }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "Faces" xml)
                  FaceDetections.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Faces.member", (FaceDetections.to_query v.faces)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("faces", (FaceDetections.to_json v.faces));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        faces =
          (FaceDetections.of_json
             (Util.of_option_exn (Json.lookup j "faces")))
      }
  end
module CreateStreamProcessorRequest =
  struct
    type t =
      {
      input: StreamProcessorInput.t ;
      output: StreamProcessorOutput.t ;
      name: String.t ;
      settings: StreamProcessorSettings.t ;
      role_arn: String.t }
    let make ~input  ~output  ~name  ~settings  ~role_arn  () =
      { input; output; name; settings; role_arn }
    let parse xml =
      Some
        {
          input =
            (Xml.required "Input"
               (Util.option_bind (Xml.member "Input" xml)
                  StreamProcessorInput.parse));
          output =
            (Xml.required "Output"
               (Util.option_bind (Xml.member "Output" xml)
                  StreamProcessorOutput.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          settings =
            (Xml.required "Settings"
               (Util.option_bind (Xml.member "Settings" xml)
                  StreamProcessorSettings.parse));
          role_arn =
            (Xml.required "RoleArn"
               (Util.option_bind (Xml.member "RoleArn" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("RoleArn", (String.to_query v.role_arn)));
           Some
             (Query.Pair
                ("Settings", (StreamProcessorSettings.to_query v.settings)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some
             (Query.Pair
                ("Output", (StreamProcessorOutput.to_query v.output)));
           Some
             (Query.Pair ("Input", (StreamProcessorInput.to_query v.input)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("role_arn", (String.to_json v.role_arn));
           Some ("settings", (StreamProcessorSettings.to_json v.settings));
           Some ("name", (String.to_json v.name));
           Some ("output", (StreamProcessorOutput.to_json v.output));
           Some ("input", (StreamProcessorInput.to_json v.input))])
    let of_json j =
      {
        input =
          (StreamProcessorInput.of_json
             (Util.of_option_exn (Json.lookup j "input")));
        output =
          (StreamProcessorOutput.of_json
             (Util.of_option_exn (Json.lookup j "output")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        settings =
          (StreamProcessorSettings.of_json
             (Util.of_option_exn (Json.lookup j "settings")));
        role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "role_arn")))
      }
  end
module ResourceInUseException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StopStreamProcessorResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RecognizeCelebritiesResponse =
  struct
    type t =
      {
      celebrity_faces: CelebrityList.t ;
      unrecognized_faces: ComparedFaceList.t ;
      orientation_correction: OrientationCorrection.t option }
    let make ?(celebrity_faces= [])  ?(unrecognized_faces= []) 
      ?orientation_correction  () =
      { celebrity_faces; unrecognized_faces; orientation_correction }
    let parse xml =
      Some
        {
          celebrity_faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "CelebrityFaces" xml)
                  CelebrityList.parse));
          unrecognized_faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "UnrecognizedFaces" xml)
                  ComparedFaceList.parse));
          orientation_correction =
            (Util.option_bind (Xml.member "OrientationCorrection" xml)
               OrientationCorrection.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.orientation_correction
              (fun f ->
                 Query.Pair
                   ("OrientationCorrection",
                     (OrientationCorrection.to_query f)));
           Some
             (Query.Pair
                ("UnrecognizedFaces.member",
                  (ComparedFaceList.to_query v.unrecognized_faces)));
           Some
             (Query.Pair
                ("CelebrityFaces.member",
                  (CelebrityList.to_query v.celebrity_faces)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.orientation_correction
              (fun f ->
                 ("orientation_correction",
                   (OrientationCorrection.to_json f)));
           Some
             ("unrecognized_faces",
               (ComparedFaceList.to_json v.unrecognized_faces));
           Some
             ("celebrity_faces", (CelebrityList.to_json v.celebrity_faces))])
    let of_json j =
      {
        celebrity_faces =
          (CelebrityList.of_json
             (Util.of_option_exn (Json.lookup j "celebrity_faces")));
        unrecognized_faces =
          (ComparedFaceList.of_json
             (Util.of_option_exn (Json.lookup j "unrecognized_faces")));
        orientation_correction =
          (Util.option_map (Json.lookup j "orientation_correction")
             OrientationCorrection.of_json)
      }
  end
module ImageTooLargeException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidImageFormatException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StartContentModerationResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module DeleteStreamProcessorResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StartCelebrityRecognitionRequest =
  struct
    type t =
      {
      video: Video.t ;
      client_request_token: String.t option ;
      notification_channel: NotificationChannel.t option ;
      job_tag: String.t option }
    let make ~video  ?client_request_token  ?notification_channel  ?job_tag 
      () = { video; client_request_token; notification_channel; job_tag }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module CompareFacesRequest =
  struct
    type t =
      {
      source_image: Image.t ;
      target_image: Image.t ;
      similarity_threshold: Float.t option }
    let make ~source_image  ~target_image  ?similarity_threshold  () =
      { source_image; target_image; similarity_threshold }
    let parse xml =
      Some
        {
          source_image =
            (Xml.required "SourceImage"
               (Util.option_bind (Xml.member "SourceImage" xml) Image.parse));
          target_image =
            (Xml.required "TargetImage"
               (Util.option_bind (Xml.member "TargetImage" xml) Image.parse));
          similarity_threshold =
            (Util.option_bind (Xml.member "SimilarityThreshold" xml)
               Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.similarity_threshold
              (fun f ->
                 Query.Pair ("SimilarityThreshold", (Float.to_query f)));
           Some (Query.Pair ("TargetImage", (Image.to_query v.target_image)));
           Some (Query.Pair ("SourceImage", (Image.to_query v.source_image)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.similarity_threshold
              (fun f -> ("similarity_threshold", (Float.to_json f)));
           Some ("target_image", (Image.to_json v.target_image));
           Some ("source_image", (Image.to_json v.source_image))])
    let of_json j =
      {
        source_image =
          (Image.of_json (Util.of_option_exn (Json.lookup j "source_image")));
        target_image =
          (Image.of_json (Util.of_option_exn (Json.lookup j "target_image")));
        similarity_threshold =
          (Util.option_map (Json.lookup j "similarity_threshold")
             Float.of_json)
      }
  end
module CompareFacesResponse =
  struct
    type t =
      {
      source_image_face: ComparedSourceImageFace.t option ;
      face_matches: CompareFacesMatchList.t ;
      unmatched_faces: CompareFacesUnmatchList.t ;
      source_image_orientation_correction: OrientationCorrection.t option ;
      target_image_orientation_correction: OrientationCorrection.t option }
    let make ?source_image_face  ?(face_matches= [])  ?(unmatched_faces= []) 
      ?source_image_orientation_correction 
      ?target_image_orientation_correction  () =
      {
        source_image_face;
        face_matches;
        unmatched_faces;
        source_image_orientation_correction;
        target_image_orientation_correction
      }
    let parse xml =
      Some
        {
          source_image_face =
            (Util.option_bind (Xml.member "SourceImageFace" xml)
               ComparedSourceImageFace.parse);
          face_matches =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceMatches" xml)
                  CompareFacesMatchList.parse));
          unmatched_faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "UnmatchedFaces" xml)
                  CompareFacesUnmatchList.parse));
          source_image_orientation_correction =
            (Util.option_bind
               (Xml.member "SourceImageOrientationCorrection" xml)
               OrientationCorrection.parse);
          target_image_orientation_correction =
            (Util.option_bind
               (Xml.member "TargetImageOrientationCorrection" xml)
               OrientationCorrection.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.target_image_orientation_correction
              (fun f ->
                 Query.Pair
                   ("TargetImageOrientationCorrection",
                     (OrientationCorrection.to_query f)));
           Util.option_map v.source_image_orientation_correction
             (fun f ->
                Query.Pair
                  ("SourceImageOrientationCorrection",
                    (OrientationCorrection.to_query f)));
           Some
             (Query.Pair
                ("UnmatchedFaces.member",
                  (CompareFacesUnmatchList.to_query v.unmatched_faces)));
           Some
             (Query.Pair
                ("FaceMatches.member",
                  (CompareFacesMatchList.to_query v.face_matches)));
           Util.option_map v.source_image_face
             (fun f ->
                Query.Pair
                  ("SourceImageFace", (ComparedSourceImageFace.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.target_image_orientation_correction
              (fun f ->
                 ("target_image_orientation_correction",
                   (OrientationCorrection.to_json f)));
           Util.option_map v.source_image_orientation_correction
             (fun f ->
                ("source_image_orientation_correction",
                  (OrientationCorrection.to_json f)));
           Some
             ("unmatched_faces",
               (CompareFacesUnmatchList.to_json v.unmatched_faces));
           Some
             ("face_matches", (CompareFacesMatchList.to_json v.face_matches));
           Util.option_map v.source_image_face
             (fun f ->
                ("source_image_face", (ComparedSourceImageFace.to_json f)))])
    let of_json j =
      {
        source_image_face =
          (Util.option_map (Json.lookup j "source_image_face")
             ComparedSourceImageFace.of_json);
        face_matches =
          (CompareFacesMatchList.of_json
             (Util.of_option_exn (Json.lookup j "face_matches")));
        unmatched_faces =
          (CompareFacesUnmatchList.of_json
             (Util.of_option_exn (Json.lookup j "unmatched_faces")));
        source_image_orientation_correction =
          (Util.option_map
             (Json.lookup j "source_image_orientation_correction")
             OrientationCorrection.of_json);
        target_image_orientation_correction =
          (Util.option_map
             (Json.lookup j "target_image_orientation_correction")
             OrientationCorrection.of_json)
      }
  end
module VideoTooLargeException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeStreamProcessorResponse =
  struct
    type t =
      {
      name: String.t option ;
      stream_processor_arn: String.t option ;
      status: StreamProcessorStatus.t option ;
      status_message: String.t option ;
      creation_timestamp: DateTime.t option ;
      last_update_timestamp: DateTime.t option ;
      input: StreamProcessorInput.t option ;
      output: StreamProcessorOutput.t option ;
      role_arn: String.t option ;
      settings: StreamProcessorSettings.t option }
    let make ?name  ?stream_processor_arn  ?status  ?status_message 
      ?creation_timestamp  ?last_update_timestamp  ?input  ?output  ?role_arn
       ?settings  () =
      {
        name;
        stream_processor_arn;
        status;
        status_message;
        creation_timestamp;
        last_update_timestamp;
        input;
        output;
        role_arn;
        settings
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          stream_processor_arn =
            (Util.option_bind (Xml.member "StreamProcessorArn" xml)
               String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StreamProcessorStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          creation_timestamp =
            (Util.option_bind (Xml.member "CreationTimestamp" xml)
               DateTime.parse);
          last_update_timestamp =
            (Util.option_bind (Xml.member "LastUpdateTimestamp" xml)
               DateTime.parse);
          input =
            (Util.option_bind (Xml.member "Input" xml)
               StreamProcessorInput.parse);
          output =
            (Util.option_bind (Xml.member "Output" xml)
               StreamProcessorOutput.parse);
          role_arn =
            (Util.option_bind (Xml.member "RoleArn" xml) String.parse);
          settings =
            (Util.option_bind (Xml.member "Settings" xml)
               StreamProcessorSettings.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.settings
              (fun f ->
                 Query.Pair
                   ("Settings", (StreamProcessorSettings.to_query f)));
           Util.option_map v.role_arn
             (fun f -> Query.Pair ("RoleArn", (String.to_query f)));
           Util.option_map v.output
             (fun f ->
                Query.Pair ("Output", (StreamProcessorOutput.to_query f)));
           Util.option_map v.input
             (fun f ->
                Query.Pair ("Input", (StreamProcessorInput.to_query f)));
           Util.option_map v.last_update_timestamp
             (fun f ->
                Query.Pair ("LastUpdateTimestamp", (DateTime.to_query f)));
           Util.option_map v.creation_timestamp
             (fun f ->
                Query.Pair ("CreationTimestamp", (DateTime.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (StreamProcessorStatus.to_query f)));
           Util.option_map v.stream_processor_arn
             (fun f -> Query.Pair ("StreamProcessorArn", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.settings
              (fun f -> ("settings", (StreamProcessorSettings.to_json f)));
           Util.option_map v.role_arn
             (fun f -> ("role_arn", (String.to_json f)));
           Util.option_map v.output
             (fun f -> ("output", (StreamProcessorOutput.to_json f)));
           Util.option_map v.input
             (fun f -> ("input", (StreamProcessorInput.to_json f)));
           Util.option_map v.last_update_timestamp
             (fun f -> ("last_update_timestamp", (DateTime.to_json f)));
           Util.option_map v.creation_timestamp
             (fun f -> ("creation_timestamp", (DateTime.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StreamProcessorStatus.to_json f)));
           Util.option_map v.stream_processor_arn
             (fun f -> ("stream_processor_arn", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        stream_processor_arn =
          (Util.option_map (Json.lookup j "stream_processor_arn")
             String.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StreamProcessorStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        creation_timestamp =
          (Util.option_map (Json.lookup j "creation_timestamp")
             DateTime.of_json);
        last_update_timestamp =
          (Util.option_map (Json.lookup j "last_update_timestamp")
             DateTime.of_json);
        input =
          (Util.option_map (Json.lookup j "input")
             StreamProcessorInput.of_json);
        output =
          (Util.option_map (Json.lookup j "output")
             StreamProcessorOutput.of_json);
        role_arn =
          (Util.option_map (Json.lookup j "role_arn") String.of_json);
        settings =
          (Util.option_map (Json.lookup j "settings")
             StreamProcessorSettings.of_json)
      }
  end
module DeleteStreamProcessorRequest =
  struct
    type t = {
      name: String.t }
    let make ~name  () = { name }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("name", (String.to_json v.name))])
    let of_json j =
      { name = (String.of_json (Util.of_option_exn (Json.lookup j "name"))) }
  end
module GetLabelDetectionResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      video_metadata: VideoMetadata.t option ;
      next_token: String.t option ;
      labels: LabelDetections.t ;
      label_model_version: String.t option }
    let make ?job_status  ?status_message  ?video_metadata  ?next_token 
      ?(labels= [])  ?label_model_version  () =
      {
        job_status;
        status_message;
        video_metadata;
        next_token;
        labels;
        label_model_version
      }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          labels =
            (Util.of_option []
               (Util.option_bind (Xml.member "Labels" xml)
                  LabelDetections.parse));
          label_model_version =
            (Util.option_bind (Xml.member "LabelModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.label_model_version
              (fun f -> Query.Pair ("LabelModelVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("Labels.member", (LabelDetections.to_query v.labels)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.label_model_version
              (fun f -> ("label_model_version", (String.to_json f)));
           Some ("labels", (LabelDetections.to_json v.labels));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        labels =
          (LabelDetections.of_json
             (Util.of_option_exn (Json.lookup j "labels")));
        label_model_version =
          (Util.option_map (Json.lookup j "label_model_version")
             String.of_json)
      }
  end
module DeleteFacesResponse =
  struct
    type t = {
      deleted_faces: FaceIdList.t }
    let make ?(deleted_faces= [])  () = { deleted_faces }
    let parse xml =
      Some
        {
          deleted_faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "DeletedFaces" xml)
                  FaceIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DeletedFaces.member",
                   (FaceIdList.to_query v.deleted_faces)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("deleted_faces", (FaceIdList.to_json v.deleted_faces))])
    let of_json j =
      {
        deleted_faces =
          (FaceIdList.of_json
             (Util.of_option_exn (Json.lookup j "deleted_faces")))
      }
  end
module ListCollectionsResponse =
  struct
    type t =
      {
      collection_ids: CollectionIdList.t ;
      next_token: String.t option ;
      face_model_versions: FaceModelVersionList.t }
    let make ?(collection_ids= [])  ?next_token  ?(face_model_versions= []) 
      () = { collection_ids; next_token; face_model_versions }
    let parse xml =
      Some
        {
          collection_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "CollectionIds" xml)
                  CollectionIdList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          face_model_versions =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceModelVersions" xml)
                  FaceModelVersionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("FaceModelVersions.member",
                   (FaceModelVersionList.to_query v.face_model_versions)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("CollectionIds.member",
                  (CollectionIdList.to_query v.collection_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("face_model_versions",
                (FaceModelVersionList.to_json v.face_model_versions));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some
             ("collection_ids", (CollectionIdList.to_json v.collection_ids))])
    let of_json j =
      {
        collection_ids =
          (CollectionIdList.of_json
             (Util.of_option_exn (Json.lookup j "collection_ids")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        face_model_versions =
          (FaceModelVersionList.of_json
             (Util.of_option_exn (Json.lookup j "face_model_versions")))
      }
  end
module StartFaceDetectionResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module SearchFacesRequest =
  struct
    type t =
      {
      collection_id: String.t ;
      face_id: String.t ;
      max_faces: Integer.t option ;
      face_match_threshold: Float.t option }
    let make ~collection_id  ~face_id  ?max_faces  ?face_match_threshold  ()
      = { collection_id; face_id; max_faces; face_match_threshold }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          face_id =
            (Xml.required "FaceId"
               (Util.option_bind (Xml.member "FaceId" xml) String.parse));
          max_faces =
            (Util.option_bind (Xml.member "MaxFaces" xml) Integer.parse);
          face_match_threshold =
            (Util.option_bind (Xml.member "FaceMatchThreshold" xml)
               Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> Query.Pair ("FaceMatchThreshold", (Float.to_query f)));
           Util.option_map v.max_faces
             (fun f -> Query.Pair ("MaxFaces", (Integer.to_query f)));
           Some (Query.Pair ("FaceId", (String.to_query v.face_id)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_match_threshold
              (fun f -> ("face_match_threshold", (Float.to_json f)));
           Util.option_map v.max_faces
             (fun f -> ("max_faces", (Integer.to_json f)));
           Some ("face_id", (String.to_json v.face_id));
           Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        face_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "face_id")));
        max_faces =
          (Util.option_map (Json.lookup j "max_faces") Integer.of_json);
        face_match_threshold =
          (Util.option_map (Json.lookup j "face_match_threshold")
             Float.of_json)
      }
  end
module ListStreamProcessorsRequest =
  struct
    type t = {
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ?next_token  ?max_results  () = { next_token; max_results }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module DeleteFacesRequest =
  struct
    type t = {
      collection_id: String.t ;
      face_ids: FaceIdList.t }
    let make ~collection_id  ~face_ids  () = { collection_id; face_ids }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          face_ids =
            (Xml.required "FaceIds"
               (Util.option_bind (Xml.member "FaceIds" xml) FaceIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("FaceIds.member", (FaceIdList.to_query v.face_ids)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("face_ids", (FaceIdList.to_json v.face_ids));
           Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        face_ids =
          (FaceIdList.of_json (Util.of_option_exn (Json.lookup j "face_ids")))
      }
  end
module DetectModerationLabelsRequest =
  struct
    type t = {
      image: Image.t ;
      min_confidence: Float.t option }
    let make ~image  ?min_confidence  () = { image; min_confidence }
    let parse xml =
      Some
        {
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse));
          min_confidence =
            (Util.option_bind (Xml.member "MinConfidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.min_confidence
              (fun f -> Query.Pair ("MinConfidence", (Float.to_query f)));
           Some (Query.Pair ("Image", (Image.to_query v.image)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.min_confidence
              (fun f -> ("min_confidence", (Float.to_json f)));
           Some ("image", (Image.to_json v.image))])
    let of_json j =
      {
        image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")));
        min_confidence =
          (Util.option_map (Json.lookup j "min_confidence") Float.of_json)
      }
  end
module IndexFacesRequest =
  struct
    type t =
      {
      collection_id: String.t ;
      image: Image.t ;
      external_image_id: String.t option ;
      detection_attributes: Attributes.t ;
      max_faces: Integer.t option ;
      quality_filter: QualityFilter.t option }
    let make ~collection_id  ~image  ?external_image_id 
      ?(detection_attributes= [])  ?max_faces  ?quality_filter  () =
      {
        collection_id;
        image;
        external_image_id;
        detection_attributes;
        max_faces;
        quality_filter
      }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse));
          external_image_id =
            (Util.option_bind (Xml.member "ExternalImageId" xml) String.parse);
          detection_attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "DetectionAttributes" xml)
                  Attributes.parse));
          max_faces =
            (Util.option_bind (Xml.member "MaxFaces" xml) Integer.parse);
          quality_filter =
            (Util.option_bind (Xml.member "QualityFilter" xml)
               QualityFilter.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.quality_filter
              (fun f ->
                 Query.Pair ("QualityFilter", (QualityFilter.to_query f)));
           Util.option_map v.max_faces
             (fun f -> Query.Pair ("MaxFaces", (Integer.to_query f)));
           Some
             (Query.Pair
                ("DetectionAttributes.member",
                  (Attributes.to_query v.detection_attributes)));
           Util.option_map v.external_image_id
             (fun f -> Query.Pair ("ExternalImageId", (String.to_query f)));
           Some (Query.Pair ("Image", (Image.to_query v.image)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.quality_filter
              (fun f -> ("quality_filter", (QualityFilter.to_json f)));
           Util.option_map v.max_faces
             (fun f -> ("max_faces", (Integer.to_json f)));
           Some
             ("detection_attributes",
               (Attributes.to_json v.detection_attributes));
           Util.option_map v.external_image_id
             (fun f -> ("external_image_id", (String.to_json f)));
           Some ("image", (Image.to_json v.image));
           Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")));
        external_image_id =
          (Util.option_map (Json.lookup j "external_image_id") String.of_json);
        detection_attributes =
          (Attributes.of_json
             (Util.of_option_exn (Json.lookup j "detection_attributes")));
        max_faces =
          (Util.option_map (Json.lookup j "max_faces") Integer.of_json);
        quality_filter =
          (Util.option_map (Json.lookup j "quality_filter")
             QualityFilter.of_json)
      }
  end
module DetectLabelsRequest =
  struct
    type t =
      {
      image: Image.t ;
      max_labels: Integer.t option ;
      min_confidence: Float.t option }
    let make ~image  ?max_labels  ?min_confidence  () =
      { image; max_labels; min_confidence }
    let parse xml =
      Some
        {
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse));
          max_labels =
            (Util.option_bind (Xml.member "MaxLabels" xml) Integer.parse);
          min_confidence =
            (Util.option_bind (Xml.member "MinConfidence" xml) Float.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.min_confidence
              (fun f -> Query.Pair ("MinConfidence", (Float.to_query f)));
           Util.option_map v.max_labels
             (fun f -> Query.Pair ("MaxLabels", (Integer.to_query f)));
           Some (Query.Pair ("Image", (Image.to_query v.image)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.min_confidence
              (fun f -> ("min_confidence", (Float.to_json f)));
           Util.option_map v.max_labels
             (fun f -> ("max_labels", (Integer.to_json f)));
           Some ("image", (Image.to_json v.image))])
    let of_json j =
      {
        image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")));
        max_labels =
          (Util.option_map (Json.lookup j "max_labels") Integer.of_json);
        min_confidence =
          (Util.option_map (Json.lookup j "min_confidence") Float.of_json)
      }
  end
module StartLabelDetectionResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module DetectLabelsResponse =
  struct
    type t =
      {
      labels: Labels.t ;
      orientation_correction: OrientationCorrection.t option ;
      label_model_version: String.t option }
    let make ?(labels= [])  ?orientation_correction  ?label_model_version  ()
      = { labels; orientation_correction; label_model_version }
    let parse xml =
      Some
        {
          labels =
            (Util.of_option []
               (Util.option_bind (Xml.member "Labels" xml) Labels.parse));
          orientation_correction =
            (Util.option_bind (Xml.member "OrientationCorrection" xml)
               OrientationCorrection.parse);
          label_model_version =
            (Util.option_bind (Xml.member "LabelModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.label_model_version
              (fun f -> Query.Pair ("LabelModelVersion", (String.to_query f)));
           Util.option_map v.orientation_correction
             (fun f ->
                Query.Pair
                  ("OrientationCorrection",
                    (OrientationCorrection.to_query f)));
           Some (Query.Pair ("Labels.member", (Labels.to_query v.labels)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.label_model_version
              (fun f -> ("label_model_version", (String.to_json f)));
           Util.option_map v.orientation_correction
             (fun f ->
                ("orientation_correction", (OrientationCorrection.to_json f)));
           Some ("labels", (Labels.to_json v.labels))])
    let of_json j =
      {
        labels =
          (Labels.of_json (Util.of_option_exn (Json.lookup j "labels")));
        orientation_correction =
          (Util.option_map (Json.lookup j "orientation_correction")
             OrientationCorrection.of_json);
        label_model_version =
          (Util.option_map (Json.lookup j "label_model_version")
             String.of_json)
      }
  end
module StartFaceSearchResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module StartStreamProcessorResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ResourceAlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DetectTextResponse =
  struct
    type t = {
      text_detections: TextDetectionList.t }
    let make ?(text_detections= [])  () = { text_detections }
    let parse xml =
      Some
        {
          text_detections =
            (Util.of_option []
               (Util.option_bind (Xml.member "TextDetections" xml)
                  TextDetectionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TextDetections.member",
                   (TextDetectionList.to_query v.text_detections)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("text_detections",
                (TextDetectionList.to_json v.text_detections))])
    let of_json j =
      {
        text_detections =
          (TextDetectionList.of_json
             (Util.of_option_exn (Json.lookup j "text_detections")))
      }
  end
module InternalServerError =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SearchFacesResponse =
  struct
    type t =
      {
      searched_face_id: String.t option ;
      face_matches: FaceMatchList.t ;
      face_model_version: String.t option }
    let make ?searched_face_id  ?(face_matches= [])  ?face_model_version  ()
      = { searched_face_id; face_matches; face_model_version }
    let parse xml =
      Some
        {
          searched_face_id =
            (Util.option_bind (Xml.member "SearchedFaceId" xml) String.parse);
          face_matches =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceMatches" xml)
                  FaceMatchList.parse));
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("FaceMatches.member",
                  (FaceMatchList.to_query v.face_matches)));
           Util.option_map v.searched_face_id
             (fun f -> Query.Pair ("SearchedFaceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> ("face_model_version", (String.to_json f)));
           Some ("face_matches", (FaceMatchList.to_json v.face_matches));
           Util.option_map v.searched_face_id
             (fun f -> ("searched_face_id", (String.to_json f)))])
    let of_json j =
      {
        searched_face_id =
          (Util.option_map (Json.lookup j "searched_face_id") String.of_json);
        face_matches =
          (FaceMatchList.of_json
             (Util.of_option_exn (Json.lookup j "face_matches")));
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json)
      }
  end
module StartCelebrityRecognitionResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module CreateStreamProcessorResponse =
  struct
    type t = {
      stream_processor_arn: String.t option }
    let make ?stream_processor_arn  () = { stream_processor_arn }
    let parse xml =
      Some
        {
          stream_processor_arn =
            (Util.option_bind (Xml.member "StreamProcessorArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stream_processor_arn
              (fun f ->
                 Query.Pair ("StreamProcessorArn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stream_processor_arn
              (fun f -> ("stream_processor_arn", (String.to_json f)))])
    let of_json j =
      {
        stream_processor_arn =
          (Util.option_map (Json.lookup j "stream_processor_arn")
             String.of_json)
      }
  end
module GetFaceSearchResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      next_token: String.t option ;
      video_metadata: VideoMetadata.t option ;
      persons: PersonMatches.t }
    let make ?job_status  ?status_message  ?next_token  ?video_metadata 
      ?(persons= [])  () =
      { job_status; status_message; next_token; video_metadata; persons }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          persons =
            (Util.of_option []
               (Util.option_bind (Xml.member "Persons" xml)
                  PersonMatches.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Persons.member", (PersonMatches.to_query v.persons)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("persons", (PersonMatches.to_json v.persons));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        persons =
          (PersonMatches.of_json
             (Util.of_option_exn (Json.lookup j "persons")))
      }
  end
module GetCelebrityInfoResponse =
  struct
    type t = {
      urls: Urls.t ;
      name: String.t option }
    let make ?(urls= [])  ?name  () = { urls; name }
    let parse xml =
      Some
        {
          urls =
            (Util.of_option []
               (Util.option_bind (Xml.member "Urls" xml) Urls.parse));
          name = (Util.option_bind (Xml.member "Name" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name
              (fun f -> Query.Pair ("Name", (String.to_query f)));
           Some (Query.Pair ("Urls.member", (Urls.to_query v.urls)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Some ("urls", (Urls.to_json v.urls))])
    let of_json j =
      {
        urls = (Urls.of_json (Util.of_option_exn (Json.lookup j "urls")));
        name = (Util.option_map (Json.lookup j "name") String.of_json)
      }
  end
module GetContentModerationResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      video_metadata: VideoMetadata.t option ;
      moderation_labels: ContentModerationDetections.t ;
      next_token: String.t option ;
      moderation_model_version: String.t option }
    let make ?job_status  ?status_message  ?video_metadata 
      ?(moderation_labels= [])  ?next_token  ?moderation_model_version  () =
      {
        job_status;
        status_message;
        video_metadata;
        moderation_labels;
        next_token;
        moderation_model_version
      }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          moderation_labels =
            (Util.of_option []
               (Util.option_bind (Xml.member "ModerationLabels" xml)
                  ContentModerationDetections.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          moderation_model_version =
            (Util.option_bind (Xml.member "ModerationModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.moderation_model_version
              (fun f ->
                 Query.Pair ("ModerationModelVersion", (String.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("ModerationLabels.member",
                  (ContentModerationDetections.to_query v.moderation_labels)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.moderation_model_version
              (fun f -> ("moderation_model_version", (String.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some
             ("moderation_labels",
               (ContentModerationDetections.to_json v.moderation_labels));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        moderation_labels =
          (ContentModerationDetections.of_json
             (Util.of_option_exn (Json.lookup j "moderation_labels")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        moderation_model_version =
          (Util.option_map (Json.lookup j "moderation_model_version")
             String.of_json)
      }
  end
module StartPersonTrackingRequest =
  struct
    type t =
      {
      video: Video.t ;
      client_request_token: String.t option ;
      notification_channel: NotificationChannel.t option ;
      job_tag: String.t option }
    let make ~video  ?client_request_token  ?notification_channel  ?job_tag 
      () = { video; client_request_token; notification_channel; job_tag }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module IdempotentParameterMismatchException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DetectTextRequest =
  struct
    type t = {
      image: Image.t }
    let make ~image  () = { image }
    let parse xml =
      Some
        {
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Image", (Image.to_query v.image)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("image", (Image.to_json v.image))])
    let of_json j =
      { image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")))
      }
  end
module SearchFacesByImageResponse =
  struct
    type t =
      {
      searched_face_bounding_box: BoundingBox.t option ;
      searched_face_confidence: Float.t option ;
      face_matches: FaceMatchList.t ;
      face_model_version: String.t option }
    let make ?searched_face_bounding_box  ?searched_face_confidence 
      ?(face_matches= [])  ?face_model_version  () =
      {
        searched_face_bounding_box;
        searched_face_confidence;
        face_matches;
        face_model_version
      }
    let parse xml =
      Some
        {
          searched_face_bounding_box =
            (Util.option_bind (Xml.member "SearchedFaceBoundingBox" xml)
               BoundingBox.parse);
          searched_face_confidence =
            (Util.option_bind (Xml.member "SearchedFaceConfidence" xml)
               Float.parse);
          face_matches =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceMatches" xml)
                  FaceMatchList.parse));
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("FaceMatches.member",
                  (FaceMatchList.to_query v.face_matches)));
           Util.option_map v.searched_face_confidence
             (fun f ->
                Query.Pair ("SearchedFaceConfidence", (Float.to_query f)));
           Util.option_map v.searched_face_bounding_box
             (fun f ->
                Query.Pair
                  ("SearchedFaceBoundingBox", (BoundingBox.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> ("face_model_version", (String.to_json f)));
           Some ("face_matches", (FaceMatchList.to_json v.face_matches));
           Util.option_map v.searched_face_confidence
             (fun f -> ("searched_face_confidence", (Float.to_json f)));
           Util.option_map v.searched_face_bounding_box
             (fun f ->
                ("searched_face_bounding_box", (BoundingBox.to_json f)))])
    let of_json j =
      {
        searched_face_bounding_box =
          (Util.option_map (Json.lookup j "searched_face_bounding_box")
             BoundingBox.of_json);
        searched_face_confidence =
          (Util.option_map (Json.lookup j "searched_face_confidence")
             Float.of_json);
        face_matches =
          (FaceMatchList.of_json
             (Util.of_option_exn (Json.lookup j "face_matches")));
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json)
      }
  end
module DetectFacesRequest =
  struct
    type t = {
      image: Image.t ;
      attributes: Attributes.t }
    let make ~image  ?(attributes= [])  () = { image; attributes }
    let parse xml =
      Some
        {
          image =
            (Xml.required "Image"
               (Util.option_bind (Xml.member "Image" xml) Image.parse));
          attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "Attributes" xml)
                  Attributes.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Attributes.member", (Attributes.to_query v.attributes)));
           Some (Query.Pair ("Image", (Image.to_query v.image)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("attributes", (Attributes.to_json v.attributes));
           Some ("image", (Image.to_json v.image))])
    let of_json j =
      {
        image = (Image.of_json (Util.of_option_exn (Json.lookup j "image")));
        attributes =
          (Attributes.of_json
             (Util.of_option_exn (Json.lookup j "attributes")))
      }
  end
module DeleteCollectionRequest =
  struct
    type t = {
      collection_id: String.t }
    let make ~collection_id  () = { collection_id }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")))
      }
  end
module DetectFacesResponse =
  struct
    type t =
      {
      face_details: FaceDetailList.t ;
      orientation_correction: OrientationCorrection.t option }
    let make ?(face_details= [])  ?orientation_correction  () =
      { face_details; orientation_correction }
    let parse xml =
      Some
        {
          face_details =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceDetails" xml)
                  FaceDetailList.parse));
          orientation_correction =
            (Util.option_bind (Xml.member "OrientationCorrection" xml)
               OrientationCorrection.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.orientation_correction
              (fun f ->
                 Query.Pair
                   ("OrientationCorrection",
                     (OrientationCorrection.to_query f)));
           Some
             (Query.Pair
                ("FaceDetails.member",
                  (FaceDetailList.to_query v.face_details)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.orientation_correction
              (fun f ->
                 ("orientation_correction",
                   (OrientationCorrection.to_json f)));
           Some ("face_details", (FaceDetailList.to_json v.face_details))])
    let of_json j =
      {
        face_details =
          (FaceDetailList.of_json
             (Util.of_option_exn (Json.lookup j "face_details")));
        orientation_correction =
          (Util.option_map (Json.lookup j "orientation_correction")
             OrientationCorrection.of_json)
      }
  end
module LimitExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListFacesResponse =
  struct
    type t =
      {
      faces: FaceList.t ;
      next_token: String.t option ;
      face_model_version: String.t option }
    let make ?(faces= [])  ?next_token  ?face_model_version  () =
      { faces; next_token; face_model_version }
    let parse xml =
      Some
        {
          faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "Faces" xml) FaceList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Faces.member", (FaceList.to_query v.faces)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.face_model_version
              (fun f -> ("face_model_version", (String.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("faces", (FaceList.to_json v.faces))])
    let of_json j =
      {
        faces =
          (FaceList.of_json (Util.of_option_exn (Json.lookup j "faces")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json)
      }
  end
module ResourceNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StartContentModerationRequest =
  struct
    type t =
      {
      video: Video.t ;
      min_confidence: Float.t option ;
      client_request_token: String.t option ;
      notification_channel: NotificationChannel.t option ;
      job_tag: String.t option }
    let make ~video  ?min_confidence  ?client_request_token 
      ?notification_channel  ?job_tag  () =
      {
        video;
        min_confidence;
        client_request_token;
        notification_channel;
        job_tag
      }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          min_confidence =
            (Util.option_bind (Xml.member "MinConfidence" xml) Float.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.min_confidence
             (fun f -> Query.Pair ("MinConfidence", (Float.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.min_confidence
             (fun f -> ("min_confidence", (Float.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        min_confidence =
          (Util.option_map (Json.lookup j "min_confidence") Float.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module DescribeCollectionRequest =
  struct
    type t = {
      collection_id: String.t }
    let make ~collection_id  () = { collection_id }
    let parse xml =
      Some
        {
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("CollectionId", (String.to_query v.collection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("collection_id", (String.to_json v.collection_id))])
    let of_json j =
      {
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")))
      }
  end
module GetLabelDetectionRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      sort_by: LabelDetectionSortBy.t option }
    let make ~job_id  ?max_results  ?next_token  ?sort_by  () =
      { job_id; max_results; next_token; sort_by }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          sort_by =
            (Util.option_bind (Xml.member "SortBy" xml)
               LabelDetectionSortBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f ->
                 Query.Pair ("SortBy", (LabelDetectionSortBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> ("sort_by", (LabelDetectionSortBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        sort_by =
          (Util.option_map (Json.lookup j "sort_by")
             LabelDetectionSortBy.of_json)
      }
  end
module StartFaceSearchRequest =
  struct
    type t =
      {
      video: Video.t ;
      client_request_token: String.t option ;
      face_match_threshold: Float.t option ;
      collection_id: String.t ;
      notification_channel: NotificationChannel.t option ;
      job_tag: String.t option }
    let make ~video  ?client_request_token  ?face_match_threshold 
      ~collection_id  ?notification_channel  ?job_tag  () =
      {
        video;
        client_request_token;
        face_match_threshold;
        collection_id;
        notification_channel;
        job_tag
      }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          face_match_threshold =
            (Util.option_bind (Xml.member "FaceMatchThreshold" xml)
               Float.parse);
          collection_id =
            (Xml.required "CollectionId"
               (Util.option_bind (Xml.member "CollectionId" xml) String.parse));
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Some
             (Query.Pair ("CollectionId", (String.to_query v.collection_id)));
           Util.option_map v.face_match_threshold
             (fun f -> Query.Pair ("FaceMatchThreshold", (Float.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Some ("collection_id", (String.to_json v.collection_id));
           Util.option_map v.face_match_threshold
             (fun f -> ("face_match_threshold", (Float.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        face_match_threshold =
          (Util.option_map (Json.lookup j "face_match_threshold")
             Float.of_json);
        collection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "collection_id")));
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module StartFaceDetectionRequest =
  struct
    type t =
      {
      video: Video.t ;
      client_request_token: String.t option ;
      notification_channel: NotificationChannel.t option ;
      face_attributes: FaceAttributes.t option ;
      job_tag: String.t option }
    let make ~video  ?client_request_token  ?notification_channel 
      ?face_attributes  ?job_tag  () =
      {
        video;
        client_request_token;
        notification_channel;
        face_attributes;
        job_tag
      }
    let parse xml =
      Some
        {
          video =
            (Xml.required "Video"
               (Util.option_bind (Xml.member "Video" xml) Video.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          notification_channel =
            (Util.option_bind (Xml.member "NotificationChannel" xml)
               NotificationChannel.parse);
          face_attributes =
            (Util.option_bind (Xml.member "FaceAttributes" xml)
               FaceAttributes.parse);
          job_tag = (Util.option_bind (Xml.member "JobTag" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> Query.Pair ("JobTag", (String.to_query f)));
           Util.option_map v.face_attributes
             (fun f ->
                Query.Pair ("FaceAttributes", (FaceAttributes.to_query f)));
           Util.option_map v.notification_channel
             (fun f ->
                Query.Pair
                  ("NotificationChannel", (NotificationChannel.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Video", (Video.to_query v.video)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_tag
              (fun f -> ("job_tag", (String.to_json f)));
           Util.option_map v.face_attributes
             (fun f -> ("face_attributes", (FaceAttributes.to_json f)));
           Util.option_map v.notification_channel
             (fun f ->
                ("notification_channel", (NotificationChannel.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("video", (Video.to_json v.video))])
    let of_json j =
      {
        video = (Video.of_json (Util.of_option_exn (Json.lookup j "video")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        notification_channel =
          (Util.option_map (Json.lookup j "notification_channel")
             NotificationChannel.of_json);
        face_attributes =
          (Util.option_map (Json.lookup j "face_attributes")
             FaceAttributes.of_json);
        job_tag = (Util.option_map (Json.lookup j "job_tag") String.of_json)
      }
  end
module IndexFacesResponse =
  struct
    type t =
      {
      face_records: FaceRecordList.t ;
      orientation_correction: OrientationCorrection.t option ;
      face_model_version: String.t option ;
      unindexed_faces: UnindexedFaces.t }
    let make ?(face_records= [])  ?orientation_correction 
      ?face_model_version  ?(unindexed_faces= [])  () =
      {
        face_records;
        orientation_correction;
        face_model_version;
        unindexed_faces
      }
    let parse xml =
      Some
        {
          face_records =
            (Util.of_option []
               (Util.option_bind (Xml.member "FaceRecords" xml)
                  FaceRecordList.parse));
          orientation_correction =
            (Util.option_bind (Xml.member "OrientationCorrection" xml)
               OrientationCorrection.parse);
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse);
          unindexed_faces =
            (Util.of_option []
               (Util.option_bind (Xml.member "UnindexedFaces" xml)
                  UnindexedFaces.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UnindexedFaces.member",
                   (UnindexedFaces.to_query v.unindexed_faces)));
           Util.option_map v.face_model_version
             (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Util.option_map v.orientation_correction
             (fun f ->
                Query.Pair
                  ("OrientationCorrection",
                    (OrientationCorrection.to_query f)));
           Some
             (Query.Pair
                ("FaceRecords.member",
                  (FaceRecordList.to_query v.face_records)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("unindexed_faces", (UnindexedFaces.to_json v.unindexed_faces));
           Util.option_map v.face_model_version
             (fun f -> ("face_model_version", (String.to_json f)));
           Util.option_map v.orientation_correction
             (fun f ->
                ("orientation_correction", (OrientationCorrection.to_json f)));
           Some ("face_records", (FaceRecordList.to_json v.face_records))])
    let of_json j =
      {
        face_records =
          (FaceRecordList.of_json
             (Util.of_option_exn (Json.lookup j "face_records")));
        orientation_correction =
          (Util.option_map (Json.lookup j "orientation_correction")
             OrientationCorrection.of_json);
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json);
        unindexed_faces =
          (UnindexedFaces.of_json
             (Util.of_option_exn (Json.lookup j "unindexed_faces")))
      }
  end
module GetCelebrityInfoRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module GetFaceDetectionRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ~job_id  ?max_results  ?next_token  () =
      { job_id; max_results; next_token }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ProvisionedThroughputExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListStreamProcessorsResponse =
  struct
    type t =
      {
      next_token: String.t option ;
      stream_processors: StreamProcessorList.t }
    let make ?next_token  ?(stream_processors= [])  () =
      { next_token; stream_processors }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          stream_processors =
            (Util.of_option []
               (Util.option_bind (Xml.member "StreamProcessors" xml)
                  StreamProcessorList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StreamProcessors.member",
                   (StreamProcessorList.to_query v.stream_processors)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stream_processors",
                (StreamProcessorList.to_json v.stream_processors));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        stream_processors =
          (StreamProcessorList.of_json
             (Util.of_option_exn (Json.lookup j "stream_processors")))
      }
  end
module GetCelebrityRecognitionRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      sort_by: CelebrityRecognitionSortBy.t option }
    let make ~job_id  ?max_results  ?next_token  ?sort_by  () =
      { job_id; max_results; next_token; sort_by }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          sort_by =
            (Util.option_bind (Xml.member "SortBy" xml)
               CelebrityRecognitionSortBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f ->
                 Query.Pair
                   ("SortBy", (CelebrityRecognitionSortBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> ("sort_by", (CelebrityRecognitionSortBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        sort_by =
          (Util.option_map (Json.lookup j "sort_by")
             CelebrityRecognitionSortBy.of_json)
      }
  end
module StartPersonTrackingResponse =
  struct
    type t = {
      job_id: String.t option }
    let make ?job_id  () = { job_id }
    let parse xml =
      Some
        { job_id = (Util.option_bind (Xml.member "JobId" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> Query.Pair ("JobId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.job_id
              (fun f -> ("job_id", (String.to_json f)))])
    let of_json j =
      { job_id = (Util.option_map (Json.lookup j "job_id") String.of_json) }
  end
module GetFaceSearchRequest =
  struct
    type t =
      {
      job_id: String.t ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      sort_by: FaceSearchSortBy.t option }
    let make ~job_id  ?max_results  ?next_token  ?sort_by  () =
      { job_id; max_results; next_token; sort_by }
    let parse xml =
      Some
        {
          job_id =
            (Xml.required "JobId"
               (Util.option_bind (Xml.member "JobId" xml) String.parse));
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          sort_by =
            (Util.option_bind (Xml.member "SortBy" xml)
               FaceSearchSortBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> Query.Pair ("SortBy", (FaceSearchSortBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Some (Query.Pair ("JobId", (String.to_query v.job_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.sort_by
              (fun f -> ("sort_by", (FaceSearchSortBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Some ("job_id", (String.to_json v.job_id))])
    let of_json j =
      {
        job_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "job_id")));
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        sort_by =
          (Util.option_map (Json.lookup j "sort_by") FaceSearchSortBy.of_json)
      }
  end
module InvalidS3ObjectException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeCollectionResponse =
  struct
    type t =
      {
      face_count: Long.t option ;
      face_model_version: String.t option ;
      collection_a_r_n: String.t option ;
      creation_timestamp: DateTime.t option }
    let make ?face_count  ?face_model_version  ?collection_a_r_n 
      ?creation_timestamp  () =
      { face_count; face_model_version; collection_a_r_n; creation_timestamp
      }
    let parse xml =
      Some
        {
          face_count =
            (Util.option_bind (Xml.member "FaceCount" xml) Long.parse);
          face_model_version =
            (Util.option_bind (Xml.member "FaceModelVersion" xml)
               String.parse);
          collection_a_r_n =
            (Util.option_bind (Xml.member "CollectionARN" xml) String.parse);
          creation_timestamp =
            (Util.option_bind (Xml.member "CreationTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.creation_timestamp
              (fun f ->
                 Query.Pair ("CreationTimestamp", (DateTime.to_query f)));
           Util.option_map v.collection_a_r_n
             (fun f -> Query.Pair ("CollectionARN", (String.to_query f)));
           Util.option_map v.face_model_version
             (fun f -> Query.Pair ("FaceModelVersion", (String.to_query f)));
           Util.option_map v.face_count
             (fun f -> Query.Pair ("FaceCount", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.creation_timestamp
              (fun f -> ("creation_timestamp", (DateTime.to_json f)));
           Util.option_map v.collection_a_r_n
             (fun f -> ("collection_a_r_n", (String.to_json f)));
           Util.option_map v.face_model_version
             (fun f -> ("face_model_version", (String.to_json f)));
           Util.option_map v.face_count
             (fun f -> ("face_count", (Long.to_json f)))])
    let of_json j =
      {
        face_count =
          (Util.option_map (Json.lookup j "face_count") Long.of_json);
        face_model_version =
          (Util.option_map (Json.lookup j "face_model_version")
             String.of_json);
        collection_a_r_n =
          (Util.option_map (Json.lookup j "collection_a_r_n") String.of_json);
        creation_timestamp =
          (Util.option_map (Json.lookup j "creation_timestamp")
             DateTime.of_json)
      }
  end
module GetCelebrityRecognitionResponse =
  struct
    type t =
      {
      job_status: VideoJobStatus.t option ;
      status_message: String.t option ;
      video_metadata: VideoMetadata.t option ;
      next_token: String.t option ;
      celebrities: CelebrityRecognitions.t }
    let make ?job_status  ?status_message  ?video_metadata  ?next_token 
      ?(celebrities= [])  () =
      { job_status; status_message; video_metadata; next_token; celebrities }
    let parse xml =
      Some
        {
          job_status =
            (Util.option_bind (Xml.member "JobStatus" xml)
               VideoJobStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          video_metadata =
            (Util.option_bind (Xml.member "VideoMetadata" xml)
               VideoMetadata.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          celebrities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Celebrities" xml)
                  CelebrityRecognitions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Celebrities.member",
                   (CelebrityRecognitions.to_query v.celebrities)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.video_metadata
             (fun f ->
                Query.Pair ("VideoMetadata", (VideoMetadata.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.job_status
             (fun f -> Query.Pair ("JobStatus", (VideoJobStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("celebrities", (CelebrityRecognitions.to_json v.celebrities));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.video_metadata
             (fun f -> ("video_metadata", (VideoMetadata.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.job_status
             (fun f -> ("job_status", (VideoJobStatus.to_json f)))])
    let of_json j =
      {
        job_status =
          (Util.option_map (Json.lookup j "job_status")
             VideoJobStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        video_metadata =
          (Util.option_map (Json.lookup j "video_metadata")
             VideoMetadata.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        celebrities =
          (CelebrityRecognitions.of_json
             (Util.of_option_exn (Json.lookup j "celebrities")))
      }
  end