program ofcreadertest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  testofcreader in 'testofcreader.pas',
  ofcreader in '..\source\ofcreader.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

