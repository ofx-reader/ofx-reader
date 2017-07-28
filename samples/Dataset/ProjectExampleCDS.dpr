program ProjectExampleCDS;

uses
  Vcl.Forms,
  ExampleClientDataset in 'ExampleClientDataset.pas' {Form1},
  ofxreader in '..\..\source\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
