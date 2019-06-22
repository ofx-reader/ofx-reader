program ProjectSimple;

uses
  {$IfDef VER210}
  Forms,
  {$Else}
  Vcl.Forms,
  {$EndIf}
  Simple in 'Simple.pas' {FormExample1},
  ofxreader in '..\..\..\src\ofxreader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormExample1, FormExample1);
  Application.Run;
end.
