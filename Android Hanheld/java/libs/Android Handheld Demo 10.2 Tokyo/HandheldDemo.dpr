program HandheldDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Androidapi.JNI.Toast in '..\Android\Androidapi.JNI.Toast.pas',
  MainUI in '..\Android Handheld Demo MainForm\MainUI.pas' {Form1},
  Androidapi.Handheld in '..\Android Handheld\Androidapi.Handheld.pas',
  Androidapi.Interop.Scanner in '..\Android Handheld\Androidapi.Interop.Scanner.pas',
  Androidapi.NativeActivitySubclass in '..\Android Handheld\Androidapi.NativeActivitySubclass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
