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
  {$IFDEF AndroidToast}
    Androidapi.JNI.Toast,
  {$ENDIF}
  FMX.Helpers.Android,
  FMX.Platform.Android,

  Androidapi.Interop.Scanner.Types,
  Androidapi.Interop.Scanner.ZebraDW,
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
{$SCOPEDENUMS ON}
  TScannerType = (No, ZXing, Honeywell70e, Honeywell75e, ZebraEMDK, ZebraDataWedge);

  TBarCodeScanner = class(THandheld)
  private
    FScanRequestCode: Integer;
    FScannerType: TScannerType;
    FZebra_EMDKManager_Created: Boolean;
    {$IFDEF BarcodeReader}
    FZebraDW: TZebraDW_BarCodeScanner;
    {$ENDIF}
    FOnScannerCompleted: TOnScannerCompleted;

    procedure ProcOnKeyDown(Keycode: Word);
    procedure SetScannerType(const Value: TScannerType);
    procedure SetOnScannerCompleted(const Value: TOnScannerCompleted);
    //ZXing
    const ScanRequestCode = 0;
    var FMessageSubscriptionID: Integer;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function OnActivityResult(RequestCode, ResultCode: Integer; Data: JIntent): Boolean;
  public
    {Public declarations}
    OnScannerStatus: TOnScannerStatus;//for now only Zebra
    property OnScannerCompleted: TOnScannerCompleted read FOnScannerCompleted write SetOnScannerCompleted;
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
  {$IFDEF AndroidToast}
  if Result then
    TAndroidHelper.Activity.startActivityForResult(Intent, RequestCode);
  {$ENDIF}
end;

procedure LaunchZXingScanner(RequestCode: Integer);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.google.zxing.client.android.SCAN'));
  Intent.setPackage(StringToJString('com.google.zxing.client.android'));
  // If you want to target QR codes
  //Intent.putExtra(StringToJString('SCAN_MODE'), StringToJString('QR_CODE_MODE'));
  {$IFDEF AndroidToast}
  if not LaunchActivityForResult(Intent, RequestCode) then
    Toast('Cannot display ZXing scanner', ShortToast);
  {$ENDIF}
end;

// This is called from the Java activity's onBarCodeCompleteNative() method
procedure onBarCodeCompleteNative(PEnv: PJNIEnv; This: JNIObject; BarCode: JNIString); cdecl;
begin
  Log.d('+onBarCodeCompleteNative');
  {$IFDEF BarcodeReader}
  if Assigned(BarCodeScanner) and Assigned(BarCodeScanner.OnScannerCompleted) Then
    BarCodeScanner.OnScannerCompleted('', JNIStringToString(PEnv, BarCode));
  {$ENDIF}
  Log.d('-onBarCodeCompleteNative');
end;

// This is called from the Java activity's onScannerStatusNative() method
procedure onScannerStatusNative(PEnv: PJNIEnv; This: JNIObject; Status: JNIString); cdecl;
begin
  Log.d('+onScannerStatusNative');
  {$IFDEF BarcodeReader}
  if Assigned(BarCodeScanner) and Assigned(BarCodeScanner.OnScannerStatus) Then
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
  FScannerType := TScannerType.No;
  FZebra_EMDKManager_Created := False;
  FScanRequestCode := 0;

  OnKeyDown := Self.ProcOnKeyDown;
  OnScannerCompleted := nil;
  OnScannerStatus := nil;

  {$IFDEF BarcodeReader}
  FZebraDW := nil;

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
end;

destructor TBarCodeScanner.Destroy;
begin
  inherited;
  {$IFDEF BarcodeReader}
  FZebraDW.Free;
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
    TScannerType.ZebraEMDK:
      begin
        //Zebra EMDKManager must be created here after registering onScannerStatusNative
        //and only one time
        if not FZebra_EMDKManager_Created then begin
          Log.d('+WA_Zebra_Create_EMDKManager');
          TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_Zebra_Create_EMDKManager;
          FZebra_EMDKManager_Created := True;
          Log.d('-WA_Zebra_Create_EMDKManager');
        end
      end;
    TScannerType.ZebraDataWedge:
      begin
        if Assigned(FZebraDW) then
          FZebraDW.Subscribe;
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
    TScannerType.ZebraEMDK:
      begin
//Java code takes care of this so no action here
//        Log.d('+WA_Zebra_Destroy_EMDKManager');
//        TJNativeActivitySubclass.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).WA_Zebra_Destroy_EMDKManager;
//        Log.d('-WA_Zebra_Destroy_EMDKManager');
      end;
    TScannerType.ZebraDataWedge:
      begin
        if Assigned(FZebraDW) then
          FZebraDW.Unsubscribe;
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
        {$IFDEF AndroidToast}
        Toast(ScanContent, LongToast);
        {$ENDIF}
        OnScannerCompleted(ScanFormat, ScanContent);
      end;
    end
    else if ResultCode = TJActivity.JavaClass.RESULT_CANCELED then
    begin
      {$IFDEF AndroidToast}
      Toast('You cancelled the scan', ShortToast);
      {$ENDIF}
    end;
    Result := True;
  end;
end;

procedure TBarCodeScanner.SetOnScannerCompleted(const Value: TOnScannerCompleted);
begin
  FOnScannerCompleted := Value;
  {$IFDEF BarcodeReader}
  if Assigned(FZebraDW) then
    FZebraDW.OnScannerCompleted := Value;
  {$ENDIF}
end;

procedure TBarCodeScanner.SetScannerType(const Value: TScannerType);
begin
  Log.d('+SetScannerType');
  if Value <> FScannerType then begin
    {$IFDEF BarcodeReader}
    DestroyDecodeManager;
    {$ENDIF}
    FScannerType := Value;
    {$IFDEF BarcodeReader}
    CreateDecodeManager;
    //For ZEBRA DataWedge
    if FScannerType = TScannerType.ZebraDataWedge then begin
      if FZebraDW = nil then
        FZebraDW := TZebraDW_BarCodeScanner.Create(OnScannerCompleted, False);
    end
    else
      FreeAndNil(FZebraDW);
    {$ENDIF}
  end;
  Log.d('-SetScannerType');
end;
{$ENDIF Android}

end.
