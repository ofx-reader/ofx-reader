unit testofxreader;

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

type
  // Test methods for OFX file
  TestTOFXReader = class(TObject)
  strict private
    FOFXReader: TOFXReader;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
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

{ TestTOFXReader }

procedure TestTOFXReader.SetUp;
var
  ReturnValue: Boolean;
begin
  FOFXReader := TOFXReader.Create(nil);
  FOFXReader.ofxFile := '..\..\ofx-files\extrato.ofx';
  ReturnValue := FOFXReader.Import;
  Assert.IsTrue(ReturnValue);
end;

procedure TestTOFXReader.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TestTOFXReader.TestImport;
begin
  Assert.AreEqual(7, FOFXReader.Count);
end;

procedure TestTOFXReader.TestBank;
begin
  Assert.AreEqual('1', FOFXReader.BankID);
end;

procedure TestTOFXReader.TestBranch;
begin
  Assert.AreEqual('1234-1', FOFXReader.BranchID);
end;

procedure TestTOFXReader.TestAccount;
begin
  Assert.AreEqual('54321-9', FOFXReader.AccountID);
end;

procedure TestTOFXReader.TestAccountType;
begin
  Assert.AreEqual('CHECKING', FOFXReader.AccountType);
end;

procedure TestTOFXReader.TestFinalBalance;
begin
  Assert.AreEqual('-10.00', FOFXReader.FinalBalance);
end;

procedure TestTOFXReader.TestMov0;
var i: Integer;
begin
  for i := 0 to FOFXReader.Count-1 do
  begin
     case i of
      0: begin
        Assert.AreEqual('OTHER', FOFXReader.Get(i).MovType);
        Assert.AreEqual('01/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        Assert.AreEqual('-10.00', FOFXReader.Get(i).Value);
        Assert.AreEqual('2016060111650', FOFXReader.Get(i).ID);
        Assert.AreEqual('000391100701', FOFXReader.Get(i).Document);
        Assert.AreEqual('Cobrança de I.O.F.', FOFXReader.Get(i).Description);
      end;
     end;
  end;
end;

procedure TestTOFXReader.TestMov1;
var i: Integer;
begin
  for i := 0 to FOFXReader.Count-1 do
  begin
     case i of
      1: begin
        Assert.AreEqual('OTHER', FOFXReader.Get(i).MovType);
        Assert.AreEqual('02/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        Assert.AreEqual('880.00', FOFXReader.Get(i).Value);
        Assert.AreEqual('2016060202176000', FOFXReader.Get(i).ID);
        Assert.AreEqual('000000121482', FOFXReader.Get(i).Document);
        Assert.AreEqual('Recebimento de Proventos', FOFXReader.Get(i).Description);
      end;
     end;
  end;
end;

procedure TestTOFXReader.TestMov4;
var i: Integer;
begin
  for i := 0 to FOFXReader.Count-1 do
  begin
     case i of
      4: begin
        Assert.AreEqual('OTHER', FOFXReader.Get(i).MovType);
        Assert.AreEqual('03/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        Assert.AreEqual('-200.00', FOFXReader.Get(i).Value);
        Assert.AreEqual('20160603149980', FOFXReader.Get(i).ID);
        Assert.AreEqual('000000141658', FOFXReader.Get(i).Document);
        Assert.AreEqual('Compra com Cartão - 03/06 11:34 LOJAS X', FOFXReader.Get(i).Description);
      end;
     end;
  end;
end;

{ TestTOFCReader }

procedure TestTOFCReader.SetUp;
var
  ReturnValue: Boolean;
begin
  FOFCReader := TOFXReader.Create(nil);
  FOFCReader.ofxFile := '..\..\ofx-files\extrato.ofc';
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
        Assert.AreEqual('Cobrança de I.O.F.', FOFCReader.Get(i).Description);
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
        Assert.AreEqual('Compra com Cartão - 03/06 11:34 LOJAS X', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TestTOFXReader);
  TDUnitX.RegisterTestFixture(TestTOFCReader);
end.

