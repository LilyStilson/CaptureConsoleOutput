unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Threading,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, FMX.Layouts, FMX.BufferedLayout, CaptureConsoleUnit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TTask.Run(procedure begin
    CaptureConsoleOutputSync({$IFDEF MSWINDOWS}'ping google.com'{$ELSE MACOS}'ping -c 4 google.com'{$ENDIF},
      procedure (const Line: String) begin
        Memo1.Lines.Add(Line);
        Memo1.GoToTextEnd;
      end);
  end);
end;

end.
