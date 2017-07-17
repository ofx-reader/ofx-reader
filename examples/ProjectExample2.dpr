program ProjectExample2;

uses
  Vcl.Forms,
  Example2 in 'Example2.pas' {Form1},
  ofxreader in '..\lib\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
