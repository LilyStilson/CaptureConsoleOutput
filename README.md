# CaptureConsoleOutput


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
