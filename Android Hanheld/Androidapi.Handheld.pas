unit Androidapi.Handheld;

interface

uses
  System.SysUtils,
{$IFDEF Android}
  Androidapi.Appglue,
  Androidapi.Input,
  FMX.Platform.Android,
  FMX.Platform,
{$ENDIF}
  FMX.Types,
  FMX.Dialogs,
  FMX.KeyMapping;

{$IFDEF Android}

type
  TOnKeyUpHandheld = procedure (Keycode: Word) of object;
  TOnKeyDownHandheld = procedure (Keycode: Word) of object;

  THandheld = class(TObject)
  private
    { Private declarations }
    function MyHandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
    procedure SetActive(const Value: Boolean);
  protected
    FActive: Boolean;
  public
    { Public declarations }
    OnKeyUp: TOnKeyUpHandheld;
    OnKeyDown: TOnKeyDownHandheld;
    function MyHandleAndroidInputEvent(const App: TAndroidApplicationGlue; const AEvent: PAInputEvent): Int32 ;
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
  end;

var
  LastInstance: THandheld;
  OldHandledAndroidInputEvent : TOnInputEvent;
{$ENDIF}

implementation

{$IFDEF Android}
constructor THandheld.Create;
begin
  inherited;

  LastInstance:=Self;
  OnKeyUp:=nil;
  OnKeyDown:=nil;

  Factive := false;
  OldHandledAndroidInputEvent := TAndroidApplicationGlue.Current.OnInputEvent;
  TAndroidApplicationGlue.Current.OnInputEvent := MyHandleAndroidInputEvent;
end;

destructor THandheld.Destroy;
begin
  TAndroidApplicationGlue.Current.OnInputEvent := OldHandledAndroidInputEvent;
  LastInstance := nil;
  inherited;
end;

function THandheld.MyHandleAndroidInputEvent(const App: TAndroidApplicationGlue;
  const AEvent: PAInputEvent): Int32;
var
  EventType: Int64;
begin
  Result := OldHandledAndroidInputEvent(App, AEvent);

  EventType := AInputEvent_getType(AEvent);
  if EventType = AINPUT_EVENT_TYPE_KEY then
    if Assigned(LastInstance) then
      LastInstance.MyHandleAndroidKeyEvent(AEvent); // Keyboard input
end;

function THandheld.MyHandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
var
  KeyCode, vkKeyCode, ScanCode: Word;
  KeyKind: TKeyKind;
  KeyMappingService: IFMXKeyMappingService;
begin
  Result := 0;

  KeyCode := AKeyEvent_getKeyCode(AEvent);
  vkKeyCode := 0;
  if TPlatformServices.Current.SupportsPlatformService(IFMXKeyMappingService, KeyMappingService) then
    vkKeyCode:= KeyMappingService.PlatformKeyToVirtualKey(KeyCode, KeyKind);
  //Read special (unknown keys)

  if (vkKeyCode = 0) and (KeyKind = TKeyKind.Unknown) then
  begin
    case AKeyEvent_getAction(AEvent) of
      AKEY_EVENT_ACTION_DOWN:
        begin
          ScanCode := AKeyEvent_getScanCode(AEvent);
          if Assigned(OnKeyDown) then OnKeyDown(Scancode);
        end;
      AKEY_EVENT_ACTION_UP:
        begin
          ScanCode := AKeyEvent_getScanCode(AEvent);
          if Assigned(OnKeyUp) then OnKeyUp(Scancode);
        end;
    end;
  end;
end;

procedure THandheld.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;
{$ENDIF}

initialization
{$IFDEF Android}
  LastInstance:=nil;
{$ENDIF}

end.
