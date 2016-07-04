program ProjectExample1;

uses
  Vcl.Forms,
  Example1 in 'Example1.pas' {FormExample1},
  ofxreader in '..\source\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormExample1, FormExample1);
  Application.Run;
end.
