program ConsoleHandleTestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  CaptureConsoleUnit in '..\CaptureConsoleUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
