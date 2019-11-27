unit Androidapi.Interop.Scanner;

interface

uses
  System.Messaging,
  System.SysUtils,
  System.Classes,
{$IFDEF Android}
  Posix.PThread,
  Androidapi.Handheld,
  Androidapi.JNIMarshal,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.NativeActivitySubclass,
  Androidapi.NativeActivity,
  Androidapi.Helpers,
  Androidapi.Appglue,
  Androidapi.Input,
  Androidapi.JNI,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.Toast,
  FMX.Helpers.Android,
  FMX.Platform.Android,
{$ENDIF}
  FMX.Dialogs,
  FMX.Types;

{$IFDEF Android}
const
  LEFT_UP_KEY = 87;
  RIGHT_UP_KEY = 88;
  CENTRAL_KEY = 148;

  ERR_CANT_DISP_BC_SCANNER = -1; // Error: Cannot display Barcode scanner

type
  TOnScannerCompleted = procedure(ScanFormat, ScanContent: string) of object;
  TOnScannerStatus = procedure(AStatus: String) of object;

{$SCOPEDENUMS ON}
  TScannerType = (No, ZXing, Honeywell70e, Honeywell75e, Zebra);

  TBarCodeScanner = class(THandheld)
  private
    FScanRequestCode: Integer;
    FScannerType: TScannerType;

    procedure ProcOnKeyDown(Keycode: Word);
    procedure SetScannerType(const Value: TScannerType);
    //ZXing
    const ScanRequestCode = 0;
    var FMessageSubscriptionID: Integer;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function OnActivityResult(RequestCode, ResultCode: Integer; Data: JIntent): Boolean;
  public
    {Public declarations}
    OnScannerCompleted: TOnScannerCompleted;
    OnScannerStatus: TOnScannerStatus;//for now only Zebra
    procedure DoTriggerScan;
    constructor Create;
    {$IFDEF BarcodeReader}
    procedure CreateDecodeManager;
    procedure DestroyDecodeManager;
    {$ENDIF}
    destructor Destroy; override;

    property ScannerType: TScannerType read FScannerType write SetScannerType;
  end;

var
  BarCodeScanner: TBarCodeScanner = nil;
{$ENDIF}

implementation

{$IFDEF Android}

function LaunchActivityForResult(const Intent: JIntent; RequestCode: Integer): Boolean;
var
  ResolveInfo: JResolveInfo;
begin
  ResolveInfo := TAndroidHelper.Activity.getPackageManager.resolveActivity(Intent, 0);
  Result := ResolveInfo <> nil;
  if Result then
    TAndroidHelper.Activity.startActivityForResult(Intent, RequestCode);
end;

procedure LaunchZXingScanner(RequestCode: Integer);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.google.zxing.client.android.SCAN'));
  Intent.setPackage(StringToJString('com.google.zxing.client.android'));
  // If you want to target QR codes
  //Intent.putExtra(StringToJString('SCAN_MODE'), StringToJString('QR_CODE_MODE'));
  if not LaunchActivityForResult(Intent, RequestCode) then
    Toast('Cannot display ZXing scanner', ShortToast);
end;

// This is called from the Java activity's onBarCodeCompleteNative() method
procedure onBarCodeCompleteNative(PEnv: PJNIEnv; This: JNIObject; BarCode: JNIString); cdecl;
begin
  Log.d('+onBarCodeCompleteNative');
  Log.d('Thread (Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x), POSIX:%.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId, GetCurrentThreadID]);
  {$IFDEF BarcodeReader}
  If Assigned(BarCodeScanner) then
    if Assigned(BarCodeScanner.OnScannerCompleted) Then
      BarCodeScanner.OnScannerCompleted('', JNIStringToString(PEnv, BarCode));
  {$ENDIF}
  Log.d('Synchronize is over');
  Log.d('-onBarCodeCompleteNative');
end;

// This is called from the Java activity's onScannerStatusNative() method
procedure onScannerStatusNative(PEnv: PJNIEnv; This: JNIObject; Status: JNIString); cdecl;
begin
  Log.d('+onScannerStatusNative');
  {$IFDEF BarcodeReader}
  If Assigned(BarCodeScanner) then
    if Assigned(BarCodeScanner.OnScannerStatus) Then
      BarCodeScanner.OnScannerStatus(JNIStringToString(PEnv, Status));
  {$ENDIF}
  Log.d('-onScannerStatusNative');
end;

constructor TBarCodeScanner.Create;
var
  PEnv: PJNIEnv;
  ActivityClass: JNIClass;
  NativeMethods: array [0 .. 1] of JNINativeMethod;
begin
  inherited;
  {$IFDEF BarcodeReader}
  Log.d('Starting the registration JNI stuff');
  PEnv := TJNIResolver.GetJNIEnv;
  Log.d('Registering interop methods');

  NativeMethods[0].Name := 'onBarCodeCompleteNative';
  NativeMethods[0].Signature := '(Ljava/lang/String;)V';
  NativeMethods[0].FnPtr := @onBarCodeCompleteNative;

//  NativeMethods[1].Name := 'onBarCodeFailNative';
//  NativeMethods[1].Signature := '()V';
//  NativeMethods[1].FnPtr := @onBarCodeFailNative;

  NativeMethods[1].Name := 'onScannerStatusNative';
  NativeMethods[1].Signature := '(Ljava/lang/String;)V';
  NativeMethods[1].FnPtr := @onScannerStatusNative;

  ActivityClass := PEnv^.GetObjectClass(PEnv, PANativeActivity(System.DelphiActivity).clazz);
  PEnv^.RegisterNatives(PEnv, ActivityClass, @NativeMethods[0], 2);//2 functions to register
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);
  {$ENDIF}

  FScannerType := TScannerType.No;

  FScanRequestCode := 0;

  OnKeyDown := Self.ProcOnKeyDown;
  OnScannerCompleted := nil;
  OnScannerStatus := nil;

  //we create Zebra EMDKManager here after registering onScannerStatusNative
  Log.d('+WA_Zebra_Create_EMDKManager');
  TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_Zebra_Create_EMDKManager;
  Log.d('-WA_Zebra_Create_EMDKManager');
end;

destructor TBarCodeScanner.Destroy;
begin
  inherited;
  {$IFDEF BarcodeReader}
  DestroyDecodeManager;
  {$ENDIF}
end;

procedure TBarCodeScanner.ProcOnKeyDown(Keycode: Word);
begin
  case FScannerType of
    TScannerType.Honeywell70e:
      if (Keycode = LEFT_UP_KEY) or (Keycode = RIGHT_UP_KEY) or (Keycode = CENTRAL_KEY) then
        DoTriggerScan;
  end;
end;

{$IFDEF BarcodeReader}
procedure TBarCodeScanner.CreateDecodeManager;
begin
  case FScannerType of
    TScannerType.Honeywell70e:
      begin
        Log.d('+WA_CreateDecodeManager');
        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_70e_CreateDecodeManager;
        Log.d('-WA_CreateDecodeManager');
      end;
    TScannerType.Honeywell75e:
      begin
        Log.d('+WA_75e_Create_aidcManager');
        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_75e_Create_aidcManager;
        Log.d('-WA_75e_Create_aidcManager');
      end;
    TScannerType.Zebra:
      begin
//Se creaza la TBarCodeScanner.Create()
//        Log.d('+WA_Zebra_Create_EMDKManager');
//        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_Zebra_Create_EMDKManager;
//        Log.d('-WA_Zebra_Create_EMDKManager');
      end;
  end;
end;
{$ENDIF}

{$IFDEF BarcodeReader}
procedure TBarCodeScanner.DestroyDecodeManager;
begin
  case FScannerType of
    TScannerType.Honeywell70e:
      begin
        Log.d('+WA_DestroyDecodeManager');
        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_70e_DestroyDecodeManager;
        Log.d('-WA_DestroyDecodeManager');
      end;
    TScannerType.Honeywell75e:
      begin
        Log.d('+WA_75e_Destroy_aidcManager');
        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_75e_Destroy_aidcManager;
        Log.d('-WA_75e_Destroy_aidcManager');
      end;
    TScannerType.Zebra:
      begin
//se elibereaza in java
//        Log.d('+WA_Zebra_Destroy_EMDKManager');
//        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_Zebra_Destroy_EMDKManager;
//        Log.d('-WA_Zebra_Destroy_EMDKManager');
      end;
  end;
end;
{$ENDIF}

procedure TBarCodeScanner.DoTriggerScan;
begin
  case FScannerType of
    TScannerType.ZXing: begin
      FMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityMessage);
      LaunchZXingScanner(ScanRequestCode);
    end;
    {$IFDEF BarcodeReader}
    TScannerType.Honeywell70e:
      begin
        Log.d('+WA_doTriggerScan');
        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_70e_DoTriggerScan;
        Log.d('-WA_doTriggerScan');
      end;
    {$ENDIF}
  end;
end;

procedure TBarCodeScanner.HandleActivityMessage(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageResultNotification then begin
    OnActivityResult(TMessageResultNotification(M).RequestCode, TMessageResultNotification(M).ResultCode,
      TMessageResultNotification(M).Value);
  end;
end;

function TBarCodeScanner.OnActivityResult(RequestCode, ResultCode: Integer; Data: JIntent): Boolean;
var
  ScanContent, ScanFormat: string;
begin
  Result := False;

  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, FMessageSubscriptionID);
  FMessageSubscriptionID := 0;

  // For more info see https://github.com/zxing/zxing/wiki/Scanning-Via-Intent
  if RequestCode = ScanRequestCode then
  begin
    if ResultCode = TJActivity.JavaClass.RESULT_OK then
    begin
      if Assigned(Data) then
      begin
        ScanContent := JStringToString(Data.getStringExtra(StringToJString('SCAN_RESULT')));
        ScanFormat := JStringToString(Data.getStringExtra(StringToJString('SCAN_RESULT_FORMAT')));
        Toast(ScanContent, LongToast);
        OnScannerCompleted(ScanFormat, ScanContent);
      end;
    end
    else if ResultCode = TJActivity.JavaClass.RESULT_CANCELED then
    begin
      Toast('You cancelled the scan', ShortToast);
    end;
    Result := True;
  end;
end;

procedure TBarCodeScanner.SetScannerType(const Value: TScannerType);
begin
  Log.d('+SetScannerType');
  Log.d('Thread (Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x), POSIX:%.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId, GetCurrentThreadID]);

  if Value <> FScannerType then begin
    {$IFDEF BarcodeReader}DestroyDecodeManager;{$ENDIF}
    FScannerType := Value;
    {$IFDEF BarcodeReader}CreateDecodeManager;{$ENDIF}
  end;

  Log.d('-SetScannerType');
end;
{$ENDIF Android}

end.