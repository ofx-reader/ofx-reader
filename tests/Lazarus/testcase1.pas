unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, ofxreader, testregistry;

type
  TTestCase1= class(TTestCase)
  strict private
    FOFXReader: TOFXReader;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestImport;
  end;

implementation

procedure TTestCase1.SetUp;
var
   ReturnValue: Boolean;
begin
    FOFXReader := TOFXReader.Create(nil);
    FOFXReader.ofxFile := '..\..\ofx-files\extrato.ofx';
    ReturnValue := FOFXReader.Import;
    CheckTrue(ReturnValue);
end;

procedure TTestCase1.TearDown;
begin
   FOFXReader.Free;
   FOFXReader := nil;
end;

procedure TTestCase1.TestImport;
begin
  CheckEquals(7, FOFXReader.Count);
end;

initialization
  RegisterTest('TTestCase1.TestImport', TTestCase1.Suite);
end.

