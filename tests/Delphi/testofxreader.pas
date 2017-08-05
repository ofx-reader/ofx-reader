unit testofxreader;

interface

uses
  TestFramework, classes, SysUtils, ofxreader;

type
  // Test methods for OFC file
  TestTOFCReader = class(TTestCase)
  strict private
    FOFCReader: TOFXReader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestImport;
    procedure TestBank;
    procedure TestBranch;
    procedure TestAccount;
    procedure TestAccountType;
    procedure TestFinalBalance;
    procedure TestMov0;
    procedure TestMov1;
    procedure TestMov4;
  end;

type
  // Test methods for OFX file
  TestTOFXReader = class(TTestCase)
  strict private
    FOFXReader: TOFXReader;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestImport;
    procedure TestBank;
    procedure TestBranch;
    procedure TestAccount;
    procedure TestAccountType;
    procedure TestFinalBalance;
    procedure TestMov0;
    procedure TestMov1;
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
  CheckTrue(ReturnValue);
end;

procedure TestTOFXReader.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TestTOFXReader.TestImport;
begin
  CheckEquals(7, FOFXReader.Count);
end;

procedure TestTOFXReader.TestBank;
begin
  CheckEquals('1', FOFXReader.BankID);
end;

procedure TestTOFXReader.TestBranch;
begin
  CheckEquals('1234-1', FOFXReader.BranchID);
end;

procedure TestTOFXReader.TestAccount;
begin
  CheckEquals('54321-9', FOFXReader.AccountID);
end;

procedure TestTOFXReader.TestAccountType;
begin
  CheckEquals('CHECKING', FOFXReader.AccountType);
end;

procedure TestTOFXReader.TestFinalBalance;
begin
  CheckEquals('-10.00', FOFXReader.FinalBalance);
end;

procedure TestTOFXReader.TestMov0;
var i: Integer;
begin
  for i := 0 to FOFXReader.Count-1 do
  begin
     case i of
      0: begin
        CheckEquals('OTHER', FOFXReader.Get(i).MovType);
        CheckEquals('01/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        CheckEquals('-10.00', FOFXReader.Get(i).Value);
        CheckEquals('2016060111650', FOFXReader.Get(i).ID);
        CheckEquals('000391100701', FOFXReader.Get(i).Document);
        CheckEquals('Cobrança de I.O.F.', FOFXReader.Get(i).Description);
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
        CheckEquals('OTHER', FOFXReader.Get(i).MovType);
        CheckEquals('02/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        CheckEquals('880.00', FOFXReader.Get(i).Value);
        CheckEquals('2016060202176000', FOFXReader.Get(i).ID);
        CheckEquals('000000121482', FOFXReader.Get(i).Document);
        CheckEquals('Recebimento de Proventos', FOFXReader.Get(i).Description);
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
        CheckEquals('OTHER', FOFXReader.Get(i).MovType);
        CheckEquals('03/06/2016', DateToStr(FOFXReader.Get(i).MovDate));
        CheckEquals('-200.00', FOFXReader.Get(i).Value);
        CheckEquals('20160603149980', FOFXReader.Get(i).ID);
        CheckEquals('000000141658', FOFXReader.Get(i).Document);
        CheckEquals('Compra com Cartão - 03/06 11:34 LOJAS X', FOFXReader.Get(i).Description);
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
  FOFCReader.ofxFile := '..\ofx-files\extrato.ofc';
  ReturnValue := FOFCReader.Import;
  CheckTrue(ReturnValue);
end;

procedure TestTOFCReader.TearDown;
begin
  FOFCReader.Free;
  FOFCReader := nil;
end;

procedure TestTOFCReader.TestImport;
begin
  CheckEquals(7, FOFCReader.Count);
end;

procedure TestTOFCReader.TestBank;
begin
  CheckEquals('001', FOFCReader.BankID);
end;

procedure TestTOFCReader.TestBranch;
begin
  CheckEquals('1234-13', FOFCReader.BranchID);
end;

procedure TestTOFCReader.TestAccount;
begin
  CheckEquals('00000543219', FOFCReader.AccountID);
end;

procedure TestTOFCReader.TestAccountType;
begin
  CheckEquals('0', FOFCReader.AccountType);
end;

procedure TestTOFCReader.TestFinalBalance;
begin
  CheckEquals('-10.00', FOFCReader.FinalBalance);
end;

procedure TestTOFCReader.TestMov0;
var i: Integer;
begin
  for i := 0 to FOFCReader.Count-1 do
  begin
     case i of
      0: begin
        CheckEquals('D', FOFCReader.Get(i).MovType);
        CheckEquals('01/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('-10.00', FOFCReader.Get(i).Value);
        CheckEquals('2016060111650', FOFCReader.Get(i).ID);
        CheckEquals('91100701', FOFCReader.Get(i).Document);
        CheckEquals('Cobrança de I.O.F.', FOFCReader.Get(i).Description);
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
        CheckEquals('C', FOFCReader.Get(i).MovType);
        CheckEquals('02/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('880.00', FOFCReader.Get(i).Value);
        CheckEquals('2016060202176000', FOFCReader.Get(i).ID);
        CheckEquals('00121482', FOFCReader.Get(i).Document);
        CheckEquals('Recebimento de Proventos', FOFCReader.Get(i).Description);
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
        CheckEquals('D', FOFCReader.Get(i).MovType);
        CheckEquals('03/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('-200.00', FOFCReader.Get(i).Value);
        CheckEquals('20160603149980', FOFCReader.Get(i).ID);
        CheckEquals('00141658', FOFCReader.Get(i).Document);
        CheckEquals('Compra com Cartão - 03/06 11:34 LOJAS X', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

initialization
  RegisterTest(TestTOFXReader.Suite);
  RegisterTest(TestTOFCReader.Suite);
end.

