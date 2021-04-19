unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging, FMX.Platform,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.WebBrowser, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,

  Androidapi.Interop.Scanner.ZebraDW;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Layout1: TLayout;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FZebra: TZebraDW_BarCodeScanner;
    procedure OnScannerCompleted(ScanFormat, ScanContent: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FZebra := TZebraDW_BarCodeScanner.Create(OnScannerCompleted);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FZebra.Free;
end;

procedure TForm1.OnScannerCompleted(ScanFormat, ScanContent: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ': ' + ScanContent);
//  Invalidate;
end;

end.

