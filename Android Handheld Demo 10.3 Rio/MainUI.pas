unit MainUI;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Platform,

  Androidapi.Interop.Scanner, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    lblBarcode: TLabel;
    Label2: TLabel;
    ScannerHoneywell70e: TSpeedButton;
    ScannerHoneywell75e: TSpeedButton;
    ScannerZebra: TSpeedButton;
    ScannerZXing: TSpeedButton;
    lblHoneywell: TLabel;
    lblZebra: TLabel;
    sbtnStartSoftwareScanner: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ScanerClick(Sender: TObject);
    procedure sbtnStartSoftwareScannerClick(Sender: TObject);
  private
    FNewBarCode: String;
    procedure StartScan;
    procedure onBarCodeComplete(ScanFormat, ScanContent: string);
    procedure onBarCodeFail;
    procedure onScannerStatus(Status: string);
    procedure SetScanner;
    function WA_ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

var
  AppEventService : IFMXApplicationEventService;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF Android}
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventService)) then
    AppeventService.SetApplicationEventHandler(WA_ApplicationEventHandler);
  {$IFDEF BarcodeReader}
  BarCodeScanner := TBarCodeScanner.Create;
  BarCodeScanner.OnScannerCompleted := onBarCodeComplete;
  BarCodeScanner.OnScannerStatus := onScannerStatus;
  {$ENDIF BarcodeReader}
  {$ENDIF}
  sbtnStartSoftwareScanner.Visible := False;
end;

procedure TForm1.onBarCodeComplete(ScanFormat, ScanContent: string);
begin
  FNewBarCode := ScanContent;
  //In 10.3 we can change UI directly because Java thread is the same with UI thread
  lblBarcode.Text := FNewBarCode;
end;

procedure TForm1.onBarCodeFail;
begin

end;

procedure TForm1.onScannerStatus(Status: string);
begin

end;

procedure TForm1.sbtnStartSoftwareScannerClick(Sender: TObject);
begin
  StartScan;
end;

procedure TForm1.ScanerClick(Sender: TObject);
begin
  SetScanner;
end;

procedure TForm1.SetScanner;
begin
  sbtnStartSoftwareScanner.Visible := ScannerZXing.IsPressed;
{$IFDEF ANDROID}
  {$IFDEF BarcodeReader}
  if ScannerZXing.IsPressed then
    BarCodeScanner.ScannerType := TScannerType.ZXing
  else begin
    if ScannerHoneywell70e.IsPressed then
      BarCodeScanner.ScannerType := TScannerType.Honeywell70e
    else if ScannerHoneywell75e.IsPressed then
      BarCodeScanner.ScannerType := TScannerType.Honeywell75e
    else if ScannerZebra.IsPressed then
      BarCodeScanner.ScannerType := TScannerType.Zebra;
  end;
  {$ENDIF BarcodeReader}
{$ENDIF}
end;

procedure TForm1.StartScan;
begin
  {$IFDEF Android}
  {$IFDEF BarcodeReader}
  BarCodeScanner.DoTriggerScan;
  {$ENDIF BarcodeReader}
  {$ENDIF}
end;

function TForm1.WA_ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := False;
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: ;
    TApplicationEvent.BecameActive: begin
      {$IFDEF Android}{$IFDEF BarcodeReader}BarCodeScanner.CreateDecodeManager{$ENDIF}{$ENDIF};
    end;
    TApplicationEvent.WillBecomeInactive:;
    TApplicationEvent.EnteredBackground: begin
      {$IFDEF Android}{$IFDEF BarcodeReader}BarCodeScanner.DestroyDecodeManager{$ENDIF}{$ENDIF};
    end;
    TApplicationEvent.WillBecomeForeground: ;
    TApplicationEvent.WillTerminate: begin
      {$IFDEF Android}{$IFDEF BarcodeReader}BarCodeScanner.DestroyDecodeManager{$ENDIF}{$ENDIF};
    end;
    TApplicationEvent.LowMemory: ;
    TApplicationEvent.TimeChange: ;
    TApplicationEvent.OpenURL: ;
  end;
end;

end.
