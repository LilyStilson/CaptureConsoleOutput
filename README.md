# CaptureConsoleOutput.pas
Procedure that executes console command and redirects it's output to the callback procedure.
Should be used only in an asynchronous context (use `TThread` or `TTask.Run()`), beucase of `TThread.Synchronize()` that is used inside the procedure.

Tested with RAD Studio 11.0 Alexandria on `Windows 11 (Version 21H2)` and `macOS 11.6.1 Big Sur`.

## Usage
```Pascal
uses
  CaptureConsoleUnit;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run(procedure begin
    CaptureConsoleOutputSync('ping google.com',
      procedure (const Line: String) begin
        Memo1.Lines.Add(Line);
        Memo1.GoToTextEnd;
      end);
  end);
end;
```
