unit TestOFXReaderUtils;

interface
uses
  DUnitX.TestFramework, classes, SysUtils, ofxreader;

type
  [TestFixture]
  TFindStringTest = class(TObject)
  private
    FOFXReader: TOFXReader;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_FindString_SubstringExists;
    [Test]
    procedure Test_FindString_SubstringDoesNotExist;
    [Test]
    procedure Test_FindString_CaseInsensitive;
    [Test]
    procedure Test_FindString_EmptySubstring;
    [Test]
    procedure Test_FindString_EmptyString;
  end;

  [TestFixture]
  TConvertDateTest = class
  private
    FOFXReader: TOFXReader;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_ConvertDate_ValidDate;
    [Test]
    procedure Test_ConvertDate_InvalidDate;
    [Test]
    procedure Test_ConvertDate_ShortString;
    [Test]
    procedure Test_ConvertDate_EmptyString;
  end;

implementation

{ TFindStringTest }

procedure TFindStringTest.Setup;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TFindStringTest.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TFindStringTest.Test_FindString_SubstringExists;
begin
  Assert.IsTrue(FOFXReader.FindString('Cookies', 'I love Cookies!'));
end;

procedure TFindStringTest.Test_FindString_SubstringDoesNotExist;
begin
  Assert.IsFalse(FOFXReader.FindString('Ice Cream', 'I love Cookies!'));
end;

procedure TFindStringTest.Test_FindString_CaseInsensitive;
begin
  Assert.IsTrue(FOFXReader.FindString('cookies', 'I love Cookies!'));
end;

procedure TFindStringTest.Test_FindString_EmptySubstring;
begin
  Assert.IsTrue(FOFXReader.FindString('', 'I love Cookies!'), 'Empty substring should be found in any string');
end;

procedure TFindStringTest.Test_FindString_EmptyString;
begin
  Assert.IsFalse(FOFXReader.FindString('Cookies', ''), 'Non-empty substring should not be found in an empty string');
end;

{ TConvertDateTest }

procedure TConvertDateTest.Setup;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TConvertDateTest.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TConvertDateTest.Test_ConvertDate_ValidDate;
begin
  Assert.AreEqual(EncodeDate(2023, 12, 25), FOFXReader.ConvertDate('25122023'));
end;

procedure TConvertDateTest.Test_ConvertDate_InvalidDate;
begin
  Assert.AreEqual<TDateTime>(0, FOFXReader.ConvertDate('99999999'));
end;

procedure TConvertDateTest.Test_ConvertDate_ShortString;
begin
  Assert.AreEqual<TDateTime>(0, FOFXReader.ConvertDate('2503'));
end;

procedure TConvertDateTest.Test_ConvertDate_EmptyString;
begin
  Assert.AreEqual<TDateTime>(0, FOFXReader.ConvertDate(''));
end;

initialization
  TDUnitX.RegisterTestFixture(TFindStringTest);
  TDUnitX.RegisterTestFixture(TConvertDateTest);
end.
