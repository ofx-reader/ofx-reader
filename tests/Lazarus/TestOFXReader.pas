unit TestOFXReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ofxreader;

type
  // Test methods for OFX file
  TTestOFXReader = class(TTestCase)
  strict private
    FOFXReader: TOFXReader;
  protected
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

{ TTestOFXReader }

procedure TTestOFXReader.SetUp;
begin
  FOFXReader := TOFXReader.Create(nil);
  FOFXReader.ofxFile := ExpandFileName('../fixtures/ofx/extrato.ofx');
  CheckTrue(FOFXReader.Import, 'Import should succeed for extrato.ofx');
end;

procedure TTestOFXReader.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

procedure TTestOFXReader.TestImport;
begin
  CheckEquals(7, FOFXReader.Count);
end;

procedure TTestOFXReader.TestBank;
begin
  CheckEquals('1', FOFXReader.BankID);
end;

procedure TTestOFXReader.TestBranch;
begin
  CheckEquals('1234-1', FOFXReader.BranchID);
end;

procedure TTestOFXReader.TestAccount;
begin
  CheckEquals('54321-9', FOFXReader.AccountID);
end;

procedure TTestOFXReader.TestAccountType;
begin
  CheckEquals('CHECKING', FOFXReader.AccountType);
end;

procedure TTestOFXReader.TestFinalBalance;
begin
  CheckEquals('-10.00', FOFXReader.FinalBalance);
end;

procedure TTestOFXReader.TestMov0;
begin
  CheckEquals('OTHER', FOFXReader.Get(0).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 1)), Double(FOFXReader.Get(0).MovDate), 0.0);
  CheckEquals('-10.00', FOFXReader.Get(0).Value);
  CheckEquals('2016060111650', FOFXReader.Get(0).ID);
  CheckEquals('000391100701', FOFXReader.Get(0).Document);
  CheckEquals('Cobran' + #231 + 'a de I.O.F.', FOFXReader.Get(0).Description);
end;

procedure TTestOFXReader.TestMov1;
begin
  CheckEquals('OTHER', FOFXReader.Get(1).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 2)), Double(FOFXReader.Get(1).MovDate), 0.0);
  CheckEquals('880.00', FOFXReader.Get(1).Value);
  CheckEquals('2016060202176000', FOFXReader.Get(1).ID);
  CheckEquals('000000121482', FOFXReader.Get(1).Document);
  CheckEquals('Recebimento de Proventos', FOFXReader.Get(1).Description);
end;

procedure TTestOFXReader.TestMov4;
begin
  CheckEquals('OTHER', FOFXReader.Get(4).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 3)), Double(FOFXReader.Get(4).MovDate), 0.0);
  CheckEquals('-200.00', FOFXReader.Get(4).Value);
  CheckEquals('20160603149980', FOFXReader.Get(4).ID);
  CheckEquals('000000141658', FOFXReader.Get(4).Document);
  CheckEquals('Compra com Cart' + #227 + 'o - 03/06 11:34 LOJAS X', FOFXReader.Get(4).Description);
end;

initialization
  RegisterTest(TTestOFXReader);
end.
