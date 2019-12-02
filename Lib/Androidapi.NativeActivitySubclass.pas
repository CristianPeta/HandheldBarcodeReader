unit Androidapi.NativeActivitySubclass;

interface

{$IFDEF Android}
{$IFDEF BarcodeReader}
uses
//  Androidapi.JNI.JavaTypes
  Androidapi.JNIBridge,
  Androidapi.JNI.App;

type
  JNativeActivitySubclass = interface;

  JNativeActivitySubclassClass = interface(JActivityClass)
  ['{829C77FB-08F1-4D19-9782-3C58EECAAAAA}']
    {Methods}
    //function init: JFMXNativeActivity; cdecl;
  end;

  [JavaSignature('com/winarhi/nativeactivitysubclass/NativeActivitySubclass')]
  JNativeActivitySubclass = interface(JActivity)
  ['{2FA559EC-D1D7-46AA-9C52-FEFC6B3AAAAA}']
    {Methods}
    procedure WA_70e_CreateDecodeManager;
    procedure WA_70e_DoTriggerScan;
    procedure WA_70e_DestroyDecodeManager;

    procedure WA_75e_Create_aidcManager;
    procedure WA_75e_Destroy_aidcManager;

    procedure WA_Zebra_Create_EMDKManager;
  end;
  TJNativeActivitySubclass = class(TJavaGenericImport<JNativeActivitySubclassClass, JNativeActivitySubclass>) end;
{$ENDIF BarcodeReader}
{$ENDIF Android}

implementation

//Are sens doar daca se creaza un Activity separat de NativeActivity
//procedure RegisterTypes;
//begin
//  TRegTypes.RegisterType('WA.NativeActivitySubclass.JNativeActivitySubclass',
//    TypeInfo(WA.NativeActivitySubclass.JNativeActivitySubclass));
//end;

initialization

//  RegisterTypes;

end.
