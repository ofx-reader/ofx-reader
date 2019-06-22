program ProjectExampleCDS;

uses
  {$IfDef VER210}
  Forms,
  {$Else}
  Vcl.Forms,
  {$EndIf}
  ExampleClientDataset in 'ExampleClientDataset.pas' {Form1},
  ofxreader in '..\..\..\src\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
