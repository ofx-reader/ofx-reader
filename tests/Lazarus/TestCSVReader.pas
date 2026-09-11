unit TestCSVReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ofxreader;

type
  // CSV parsing depends on src/uExtratoCsvReader.pas, which is only compiled
  // into Delphi builds ({$IFNDEF FPC}). On FPC/Lazarus, TOFXReader.Import
  // must reject .csv files with a clear error instead of silently failing.
  TTestCSVReader = class(TTestCase)
  strict private
    FOFXReader: TOFXReader;
    procedure ImportCSVFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCSVImportNotSupported;
  end;

implementation

procedure TTestCSVReader.SetUp;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TTestCSVReader.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TTestCSVReader.ImportCSVFile;
begin
  FOFXReader.OFXFile := '..\fixtures\csv\banco-brasil.csv';
  FOFXReader.Import;
end;

procedure TTestCSVReader.TestCSVImportNotSupported;
begin
  AssertException('CSV import must raise an explicit exception on FPC/Lazarus builds',
    Exception, @ImportCSVFile);
end;

initialization
  RegisterTest(TTestCSVReader);
end.
