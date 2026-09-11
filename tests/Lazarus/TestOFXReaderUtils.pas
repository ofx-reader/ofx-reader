unit TestOFXReaderUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ofxreader;

type
  TTestFindString = class(TTestCase)
  private
    FOFXReader: TOFXReader;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_FindString_SubstringExists;
    procedure Test_FindString_SubstringDoesNotExist;
    procedure Test_FindString_CaseInsensitive;
    procedure Test_FindString_EmptySubstring;
    procedure Test_FindString_EmptyString;
  end;

  TTestConvertDate = class(TTestCase)
  private
    FOFXReader: TOFXReader;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_ConvertDate_ValidDate;
    procedure Test_ConvertDate_InvalidDate;
    procedure Test_ConvertDate_ShortString;
    procedure Test_ConvertDate_EmptyString;
  end;

implementation

{ TTestFindString }

procedure TTestFindString.SetUp;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TTestFindString.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TTestFindString.Test_FindString_SubstringExists;
begin
  CheckTrue(FOFXReader.FindString('Cookies', 'I love Cookies!'));
end;

procedure TTestFindString.Test_FindString_SubstringDoesNotExist;
begin
  CheckFalse(FOFXReader.FindString('Ice Cream', 'I love Cookies!'));
end;

procedure TTestFindString.Test_FindString_CaseInsensitive;
begin
  CheckTrue(FOFXReader.FindString('cookies', 'I love Cookies!'));
end;

procedure TTestFindString.Test_FindString_EmptySubstring;
begin
  CheckTrue(FOFXReader.FindString('', 'I love Cookies!'), 'Empty substring should be found in any string');
end;

procedure TTestFindString.Test_FindString_EmptyString;
begin
  CheckFalse(FOFXReader.FindString('Cookies', ''), 'Non-empty substring should not be found in an empty string');
end;

{ TTestConvertDate }

procedure TTestConvertDate.SetUp;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TTestConvertDate.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TTestConvertDate.Test_ConvertDate_ValidDate;
begin
  CheckEquals(Double(EncodeDate(2023, 12, 25)), Double(FOFXReader.ConvertDate('25122023')), 0.0);
end;

procedure TTestConvertDate.Test_ConvertDate_InvalidDate;
begin
  CheckEquals(0.0, Double(FOFXReader.ConvertDate('99999999')), 0.0);
end;

procedure TTestConvertDate.Test_ConvertDate_ShortString;
begin
  CheckEquals(0.0, Double(FOFXReader.ConvertDate('2503')), 0.0);
end;

procedure TTestConvertDate.Test_ConvertDate_EmptyString;
begin
  CheckEquals(0.0, Double(FOFXReader.ConvertDate('')), 0.0);
end;

initialization
  RegisterTest(TTestFindString);
  RegisterTest(TTestConvertDate);
end.
