program ProjectJSON;

uses
  Vcl.Forms,
  ExampleJSON in 'ExampleJSON.pas' {FormExample1},
  ofxreader in '..\..\..\src\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormExample1, FormExample1);
  Application.Run;
end.
