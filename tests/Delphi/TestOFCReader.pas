unit TestOFCReader;

interface
uses
  DUnitX.TestFramework, classes, SysUtils, ofxreader;

type
  // Test methods for OFC file
  [TestFixture]
  TestTOFCReader = class(TObject)
  strict private
    FOFCReader: TOFXReader;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  published
    [Test]
    procedure TestImport;
    [Test]
    procedure TestBank;
    [Test]
    procedure TestBranch;
    [Test]
    procedure TestAccount;
    [Test]
    procedure TestAccountType;
    [Test]
    procedure TestFinalBalance;
    [Test]
    procedure TestMov0;
    [Test]
    procedure TestMov1;
    [Test]
    procedure TestMov4;
  end;

implementation

{ TestTOFCReader }

procedure TestTOFCReader.SetUp;
var
  ReturnValue: Boolean;
begin
  FOFCReader := TOFXReader.Create(nil);
  FOFCReader.ofxFile := '..\fixtures\ofc\extrato.ofc';
  ReturnValue := FOFCReader.Import;
  Assert.IsTrue(ReturnValue);
end;

procedure TestTOFCReader.TearDown;
begin
  FOFCReader.Free;
  FOFCReader := nil;
end;

procedure TestTOFCReader.TestImport;
begin
  Assert.AreEqual(7, FOFCReader.Count);
end;

procedure TestTOFCReader.TestBank;
begin
  Assert.AreEqual('001', FOFCReader.BankID);
end;

procedure TestTOFCReader.TestBranch;
begin
  Assert.AreEqual('1234-13', FOFCReader.BranchID);
end;

procedure TestTOFCReader.TestAccount;
begin
  Assert.AreEqual('00000543219', FOFCReader.AccountID);
end;

procedure TestTOFCReader.TestAccountType;
begin
  Assert.AreEqual('0', FOFCReader.AccountType);
end;

procedure TestTOFCReader.TestFinalBalance;
begin
  Assert.AreEqual('-10.00', FOFCReader.FinalBalance);
end;

procedure TestTOFCReader.TestMov0;
var i: Integer;
begin
  for i := 0 to FOFCReader.Count-1 do
  begin
     case i of
      0: begin
        Assert.AreEqual('D', FOFCReader.Get(i).MovType);
        Assert.AreEqual('01/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        Assert.AreEqual('-10.00', FOFCReader.Get(i).Value);
        Assert.AreEqual('2016060111650', FOFCReader.Get(i).ID);
        Assert.AreEqual('91100701', FOFCReader.Get(i).Document);
        Assert.AreEqual('Cobran' + #231 + 'a de I.O.F.', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

procedure TestTOFCReader.TestMov1;
var i: Integer;
begin
  for i := 0 to FOFCReader.Count-1 do
  begin
     case i of
      1: begin
        Assert.AreEqual('C', FOFCReader.Get(i).MovType);
        Assert.AreEqual('02/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        Assert.AreEqual('880.00', FOFCReader.Get(i).Value);
        Assert.AreEqual('2016060202176000', FOFCReader.Get(i).ID);
        Assert.AreEqual('00121482', FOFCReader.Get(i).Document);
        Assert.AreEqual('Recebimento de Proventos', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

procedure TestTOFCReader.TestMov4;
var i: Integer;
begin
  for i := 0 to FOFCReader.Count-1 do
  begin
     case i of
      4: begin
        Assert.AreEqual('D', FOFCReader.Get(i).MovType);
        Assert.AreEqual('03/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        Assert.AreEqual('-200.00', FOFCReader.Get(i).Value);
        Assert.AreEqual('20160603149980', FOFCReader.Get(i).ID);
        Assert.AreEqual('00141658', FOFCReader.Get(i).Document);
        Assert.AreEqual('Compra com Cart' + #227 + 'o - 03/06 11:34 LOJAS X', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TestTOFCReader);
end.
