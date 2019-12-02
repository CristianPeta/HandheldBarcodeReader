program HandheldDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUI in '..\Demo MainForm\MainUI.pas' {Form1},
  Androidapi.NativeActivitySubclass in '..\Lib\Androidapi.NativeActivitySubclass.pas',
  Androidapi.Handheld in '..\Lib\Androidapi.Handheld.pas',
  Androidapi.Interop.Scanner in '..\Lib\Androidapi.Interop.Scanner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
