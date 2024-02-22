unit Androidapi.Interop.Scanner.ZXingDelphi;

interface

uses
  System.Classes,
  System.Messaging,
  System.SysUtils,
  System.Permissions,
  System.Types,
  System.UITypes,
{$IFDEF Android}
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
{$ENDIF}

  FMX.Media,
  FMX.Graphics,
  FMX.DialogService,
{$IFDEF ZXingDelphi}
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager,
{$ENDIF}
  Androidapi.Interop.Scanner.Types;

{$IFDEF ZXingDelphi}
type
  TZXingDelphi_BarCodeScanner = class
  private
    FScanStatus: String;
    FScanInProgress: Boolean;
    FFrameTake: Integer;
    FScanBitmap: TBitmap;
    FPermissionCamera: String;
    FImageBitmap: TBitmap;
    FCameraComponent: TCameraComponent;
    FOnScannerCompleted: TOnScannerCompleted;
    procedure CameraPermissionRequestResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    procedure ExplainReason(Sender: TObject; const APermissions: TClassicStringDynArray;
      const APostRationaleProc: TProc);
    procedure CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
    procedure ParseImage();
    function BarCodeFormatToStr(AFormat: TBarcodeFormat): String;
  public
    constructor Create(AOnScannerCompleted: TOnScannerCompleted; AImageBitmap: TBitmap);
    destructor Destroy; override;
    procedure StartCamera;
    procedure StopCamera;
    property OnScannerCompleted: TOnScannerCompleted read FOnScannerCompleted write FOnScannerCompleted;
    property ScanStatus: String read FScanStatus;
  end;
{$ENDIF}

implementation

{$IFDEF ZXingDelphi}
{ TZXingDelphi_BarCodeScanner }

function TZXingDelphi_BarCodeScanner.BarCodeFormatToStr(AFormat: TBarcodeFormat): String;
begin
  case AFormat of
    Auto: Result := 'Auto';
    AZTEC: Result := 'AZTEC';
    CODABAR: Result := 'CODABAR';
    CODE_39: Result := 'CODE_39';
    CODE_93: Result := 'CODE_93';
    CODE_128: Result := 'CODE_128';
    DATA_MATRIX: Result := 'DATA_MATRIX';
    EAN_8: Result := 'EAN_8';
    EAN_13: Result := 'EAN_13';
    ITF: Result := 'ITF';
    MAXICODE: Result := 'MAXICODE';
    PDF_417: Result := 'PDF_417';
    QR_CODE: Result := 'QR_CODE';
    RSS_14: Result := 'RSS_14';
    RSS_EXPANDED: Result := 'RSS_EXPANDED';
    UPC_A: Result := 'UPC_A';
    UPC_E: Result := 'UPC_E';
    UPC_EAN_EXTENSION: Result := 'UPC_EAN_EXTENSION';
    MSI: Result := 'MSI';
    PLESSEY: Result := 'PLESSEY';
  else
    Result := '???';
  end;
end;

procedure TZXingDelphi_BarCodeScanner.CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread,
  procedure
  begin
    FCameraComponent.SampleBufferToBitmap(FImageBitmap, True);

    if (FScanInProgress) then
      Exit;

    { This code will take every 4 frame. }
    inc(FFrameTake);
    if (FFrameTake mod 4 <> 0) then
      Exit;

    if Assigned(FScanBitmap) then
      FreeAndNil(FScanBitmap);

    FScanBitmap := TBitmap.Create();
    FScanBitmap.Assign(FImageBitmap);

    ParseImage();
  end);
end;

procedure TZXingDelphi_BarCodeScanner.CameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) = 1)
  and (AGrantResults[0] = TPermissionStatus.Granted) then begin
    FCameraComponent.Active := false;
    FCameraComponent.Quality := FMX.Media.TVideoCaptureQuality.MediumQuality;
    FCameraComponent.Kind := FMX.Media.TCameraKind.BackCamera;
    FCameraComponent.FocusMode := FMX.Media.TFocusMode.ContinuousAutoFocus;
    FCameraComponent.Active := True;
    FScanStatus := '';
  end
  else
    TDialogService.ShowMessage
      ('Cannot scan for barcodes because the required permissions is not granted')
end;

constructor TZXingDelphi_BarCodeScanner.Create(AOnScannerCompleted: TOnScannerCompleted; AImageBitmap: TBitmap);
begin
  OnScannerCompleted := AOnScannerCompleted;
  FImageBitmap := AImageBitmap;
  FCameraComponent := TCameraComponent.Create(nil);
  FCameraComponent.OnSampleBufferReady := CameraComponentSampleBufferReady;
  {$IFDEF ANDROID}
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  {$ENDIF}
end;

destructor TZXingDelphi_BarCodeScanner.Destroy;
begin
  StopCamera;
  FCameraComponent.Free;
  if Assigned(FScanBitmap) then
    FreeAndNil(FScanBitmap);
  inherited;
end;

procedure TZXingDelphi_BarCodeScanner.ExplainReason(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  TDialogService.ShowMessage
    ('The app needs to access the camera to scan barcodes ...',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TZXingDelphi_BarCodeScanner.ParseImage;
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      ReadResult: TReadResult;
      ScanManager: TScanManager;

    begin
      fScanInProgress := True;
      ScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);

      try

        try
          ReadResult := ScanManager.Scan(fScanBitmap);
        except
          on E: Exception do
          begin
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                FScanStatus := E.Message;
              end);
            exit;
          end;
        end;

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin

            if (Length(FScanStatus) > 10) then
            begin
              FScanStatus := '*';
            end;

            FScanStatus := FScanStatus + '*';
            if (ReadResult <> nil) then
            begin
              OnScannerCompleted(BarCodeFormatToStr(ReadResult.BarcodeFormat), ReadResult.Text);
            end;

          end);

      finally
        if ReadResult <> nil then
          FreeAndNil(ReadResult);

        ScanManager.Free;
        fScanInProgress := false;
      end;

    end).Start();
end;

procedure TZXingDelphi_BarCodeScanner.StartCamera;
begin
  PermissionsService.RequestPermissions([FPermissionCamera], CameraPermissionRequestResult, ExplainReason);
end;

procedure TZXingDelphi_BarCodeScanner.StopCamera;
begin
  FCameraComponent.Active := false;
end;
{$ENDIF}

end.
