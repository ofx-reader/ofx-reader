program ofxreadertest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  testofxreader in 'testofxreader.pas',
  ofxreader in '..\source\ofxreader.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

