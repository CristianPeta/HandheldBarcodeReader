unit Androidapi.Interop.Scanner.ZebraDW;
// Into the AndroidManifest.template.xml must be specified the intent:
//          <action android:name="%package%.ZebraDW.ACTION" />
//          <category android:name="android.intent.category.DEFAULT" />

//https://techdocs.zebra.com/datawedge/8-2/guide/api/setconfig/

interface

{$IFDEF Android}
uses
  System.Messaging,
  System.SysUtils,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Fmx.Platform.Android,

  Androidapi.Interop.Scanner.Types;

type
  TZebraDW_BarCodeScanner = class
  private
    FIntentActionName: JString;
    FMessageSubscriptionID: Integer;
    FOnScannerCompleted: TOnScannerCompleted;
    FAutoSubscribe: Boolean;
    procedure CreateDataWedgeConfig;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function HandleIntentAction(const Data: JIntent): Boolean;
    procedure sendDataWedgeIntentWithExtra(action, extraKey: JString; extras: JBundle); overload;
    procedure sendDataWedgeIntentWithExtra(action, extraKey, extraValue: JString); overload;
  public
    property OnScannerCompleted: TOnScannerCompleted read FOnScannerCompleted write FOnScannerCompleted;
    constructor Create(AOnScannerCompleted: TOnScannerCompleted; AutoSubscribe: Boolean = True);
    destructor Destroy; override;
    procedure Subscribe;
    procedure Unsubscribe;
  end;
{$ENDIF}

implementation

{$IFDEF Android}
const
  // DataWedge Extras
  EXTRA_GET_VERSION_INFO = 'com.symbol.datawedge.api.GET_VERSION_INFO';
  EXTRA_CREATE_PROFILE = 'com.symbol.datawedge.api.CREATE_PROFILE';
  EXTRA_KEY_APPLICATION_NAME = 'com.symbol.datawedge.api.APPLICATION_NAME';
  EXTRA_KEY_NOTIFICATION_TYPE = 'com.symbol.datawedge.api.NOTIFICATION_TYPE';
  EXTRA_SOFT_SCAN_TRIGGER = 'com.symbol.datawedge.api.SOFT_SCAN_TRIGGER';
  EXTRA_RESULT_NOTIFICATION = 'com.symbol.datawedge.api.NOTIFICATION';
  EXTRA_REGISTER_NOTIFICATION = 'com.symbol.datawedge.api.REGISTER_FOR_NOTIFICATION';
  EXTRA_UNREGISTER_NOTIFICATION = 'com.symbol.datawedge.api.UNREGISTER_FOR_NOTIFICATION';
  EXTRA_SET_CONFIG = 'com.symbol.datawedge.api.SET_CONFIG';

  EXTRA_RESULT_NOTIFICATION_TYPE = 'NOTIFICATION_TYPE';
  EXTRA_KEY_VALUE_SCANNER_STATUS = 'SCANNER_STATUS';
  EXTRA_KEY_VALUE_PROFILE_SWITCH = 'PROFILE_SWITCH';
  EXTRA_KEY_VALUE_CONFIGURATION_UPDATE = 'CONFIGURATION_UPDATE';
  EXTRA_KEY_VALUE_NOTIFICATION_STATUS = 'STATUS';
  EXTRA_KEY_VALUE_NOTIFICATION_PROFILE_NAME = 'PROFILE_NAME';
  EXTRA_SEND_RESULT = 'SEND_RESULT';

  EXTRA_EMPTY = '';

  EXTRA_RESULT_GET_VERSION_INFO = 'com.symbol.datawedge.api.RESULT_GET_VERSION_INFO';
  EXTRA_RESULT = 'RESULT';
  EXTRA_RESULT_INFO = 'RESULT_INFO';
  EXTRA_COMMAND = 'COMMAND';

  // DataWedge Actions
  ACTION_DATAWEDGE = 'com.symbol.datawedge.api.ACTION';
  ACTION_RESULT_NOTIFICATION = 'com.symbol.datawedge.api.NOTIFICATION_ACTION';
  ACTION_RESULT = 'com.symbol.datawedge.api.RESULT_ACTION';

{ TZebraDW_BarCodeScanner }

constructor TZebraDW_BarCodeScanner.Create(AOnScannerCompleted: TOnScannerCompleted; AutoSubscribe: Boolean);
begin
  OnScannerCompleted := AOnScannerCompleted;
  FMessageSubscriptionID := 0;
  FAutoSubscribe := AutoSubscribe;

  // Register the type of intent action that we want to be able to receive.
  // Note: A corresponding <action> tag must also exist in the <intent-filter> section of AndroidManifest.template.xml.
  FIntentActionName := TAndroidHelper.Context.getPackageName.concat(StringToJString('.ZebraDW.ACTION'));
  MainActivity.registerIntentAction(FIntentActionName);
  if FAutoSubscribe then
    Subscribe;

  CreateDataWedgeConfig;
end;

destructor TZebraDW_BarCodeScanner.Destroy;
begin
  if FAutoSubscribe then
    Unsubscribe;
  inherited;
end;

procedure TZebraDW_BarCodeScanner.CreateDataWedgeConfig;
var
  packageName: JString;
  profileName: JString;
  profileConfig: JBundle;
  keystrokeConfig: JBundle;
  keystrokeProps: JBundle;
  barcodeConfig: JBundle;
  barcodeProps: JBundle;
  appConfig: JBundle;
  activityList: TJavaObjectArray<JString>;
  appConfigList: TJavaObjectArray<JBundle>;
  intentConfig: JBundle;
  intentProps: JBundle;
begin
  packageName := TAndroidHelper.Context.getPackageName;
  profileName := packageName.substring(packageName.lastIndexOf(Ord('.')) + 1);
  // Send DataWedge intent with extra to create profile
  // Use CREATE_PROFILE: http://techdocs.zebra.com/datawedge/latest/guide/api/createprofile/
  sendDataWedgeIntentWithExtra(
    StringToJString(ACTION_DATAWEDGE),
    StringToJString('com.symbol.datawedge.api.CREATE_PROFILE'),
    profileName);

  // Configure created profile to apply to this app
  profileConfig := TJBundle.JavaClass.init;
  profileConfig.putString(StringToJString('PROFILE_NAME'), profileName);
  profileConfig.putString(StringToJString('PROFILE_ENABLED'), StringToJString('true'));
  profileConfig.putString(StringToJString('CONFIG_MODE'), StringToJString('CREATE_IF_NOT_EXIST'));  // Create profile if it does not exist

  // Configure barcode input plugin
  barcodeConfig := TJBundle.JavaClass.init;
  barcodeConfig.putString(StringToJString('PLUGIN_NAME'), StringToJString('BARCODE'));
  barcodeConfig.putString(StringToJString('RESET_CONFIG'), StringToJString('true')); //  This is the default
  barcodeProps := TJBundle.JavaClass.init;
  barcodeConfig.putBundle(StringToJString('PARAM_LIST'), barcodeProps);
  profileConfig.putBundle(StringToJString('PLUGIN_CONFIG'), barcodeConfig);

  // Associate profile with this app
  appConfig := TJBundle.JavaClass.init;
  appConfig.putString(StringToJString('PACKAGE_NAME'), packageName);
  activityList := TJavaObjectArray<JString>.Create(1);
  activityList.Items[0] := StringToJString('*');
  appConfig.putStringArray(StringToJString('ACTIVITY_LIST'), activityList);
  appConfigList := TJavaObjectArray<JBundle>.Create(1);
  appConfigList.Items[0] := appConfig;
  profileConfig.putParcelableArray(StringToJString('APP_LIST'), TJavaObjectArray<JParcelable>(appConfigList));
  profileConfig.remove(StringToJString('PLUGIN_CONFIG'));
  // Use SET_CONFIG: http://techdocs.zebra.com/datawedge/latest/guide/api/setconfig/
  sendDataWedgeIntentWithExtra(StringToJString(ACTION_DATAWEDGE), StringToJString(EXTRA_SET_CONFIG), profileConfig);

  // Configure keystroke output plugin
  keystrokeConfig := TJBundle.JavaClass.init;
  keystrokeConfig.putString(StringToJString('PLUGIN_NAME'), StringToJString('KEYSTROKE'));
  keystrokeProps := TJBundle.JavaClass.init;
  keystrokeProps.putString(StringToJString('keystroke_output_enabled'), StringToJString('false'));
  keystrokeConfig.putBundle(StringToJString('PARAM_LIST'), keystrokeProps);
  profileConfig.putBundle(StringToJString('PLUGIN_CONFIG'), keystrokeConfig);
  sendDataWedgeIntentWithExtra(StringToJString(ACTION_DATAWEDGE), StringToJString(EXTRA_SET_CONFIG), profileConfig);

  // Configure intent output for captured data to be sent to this app
  intentConfig := TJBundle.JavaClass.init;
  intentConfig.putString(StringToJString('PLUGIN_NAME'), StringToJString('INTENT'));
  intentConfig.putString(StringToJString('RESET_CONFIG'), StringToJString('true'));
  intentProps := TJBundle.JavaClass.init;
  intentProps.putString(StringToJString('intent_output_enabled'), StringToJString('true'));
  intentProps.putString(StringToJString('intent_action'), FIntentActionName);//from manifest: <action android:name="%package%.ZebraDW.ACTION" />
  intentProps.putString(StringToJString('intent_category'), StringToJString('android.intent.category.DEFAULT'));
  intentProps.putString(StringToJString('intent_delivery'), StringToJString('0'));//0 = Start Activity
  intentConfig.putBundle(StringToJString('PARAM_LIST'), intentProps);
  profileConfig.putBundle(StringToJString('PLUGIN_CONFIG'), intentConfig);
  sendDataWedgeIntentWithExtra(StringToJString(ACTION_DATAWEDGE), StringToJString(EXTRA_SET_CONFIG), profileConfig);
end;

procedure TZebraDW_BarCodeScanner.HandleActivityMessage(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageReceivedNotification then
    HandleIntentAction(TMessageReceivedNotification(M).Value);
end;

function TZebraDW_BarCodeScanner.HandleIntentAction(const Data: JIntent): Boolean;
var
  JStr: JString;
begin
  Result := False;
  if (Data <> nil) and Data.getAction.equals(FIntentActionName) then
  begin
    JStr := Data.getStringExtra(StringToJString('com.symbol.datawedge.data_string'));
    OnScannerCompleted('', JStringToString(JStr));
  end;
end;

procedure TZebraDW_BarCodeScanner.sendDataWedgeIntentWithExtra(action, extraKey, extraValue: JString);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(action);
  Intent.putExtra(extraKey, extraValue);
  TAndroidHelper.Activity.sendBroadcast(Intent);
end;

procedure TZebraDW_BarCodeScanner.Subscribe;
begin
  if FMessageSubscriptionID = 0  then begin
    FMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, HandleActivityMessage);
  end;
end;

procedure TZebraDW_BarCodeScanner.Unsubscribe;
begin
  if FMessageSubscriptionID <> 0  then begin
    TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, FMessageSubscriptionID);
    FMessageSubscriptionID := 0;
  end;
end;

procedure TZebraDW_BarCodeScanner.sendDataWedgeIntentWithExtra(action, extraKey: JString; extras: JBundle);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(action);
  Intent.putExtra(extraKey, extras);
  TAndroidHelper.Activity.sendBroadcast(Intent);
end;
{$ENDIF}

end.
