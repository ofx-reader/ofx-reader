unit TestOFCReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ofxreader;

type
  // Test methods for OFC file
  TTestOFCReader = class(TTestCase)
  strict private
    FOFCReader: TOFXReader;
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

{ TTestOFCReader }

procedure TTestOFCReader.SetUp;
begin
  FOFCReader := TOFXReader.Create(nil);
  FOFCReader.ofxFile := ExpandFileName('../fixtures/ofc/extrato.ofc');
  CheckTrue(FOFCReader.Import, 'Import should succeed for extrato.ofc');
end;

procedure TTestOFCReader.TearDown;
begin
  FOFCReader.Free;
  FOFCReader := nil;
end;

procedure TTestOFCReader.TestImport;
begin
  CheckEquals(7, FOFCReader.Count);
end;

procedure TTestOFCReader.TestBank;
begin
  CheckEquals('001', FOFCReader.BankID);
end;

procedure TTestOFCReader.TestBranch;
begin
  CheckEquals('1234-13', FOFCReader.BranchID);
end;

procedure TTestOFCReader.TestAccount;
begin
  CheckEquals('00000543219', FOFCReader.AccountID);
end;

procedure TTestOFCReader.TestAccountType;
begin
  CheckEquals('0', FOFCReader.AccountType);
end;

procedure TTestOFCReader.TestFinalBalance;
begin
  CheckEquals('-10.00', FOFCReader.FinalBalance);
end;

procedure TTestOFCReader.TestMov0;
begin
  CheckEquals('D', FOFCReader.Get(0).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 1)), Double(FOFCReader.Get(0).MovDate), 0.0);
  CheckEquals('-10.00', FOFCReader.Get(0).Value);
  CheckEquals('2016060111650', FOFCReader.Get(0).ID);
  CheckEquals('91100701', FOFCReader.Get(0).Document);
  CheckEquals('Cobran' + #231 + 'a de I.O.F.', FOFCReader.Get(0).Description);
end;

procedure TTestOFCReader.TestMov1;
begin
  CheckEquals('C', FOFCReader.Get(1).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 2)), Double(FOFCReader.Get(1).MovDate), 0.0);
  CheckEquals('880.00', FOFCReader.Get(1).Value);
  CheckEquals('2016060202176000', FOFCReader.Get(1).ID);
  CheckEquals('00121482', FOFCReader.Get(1).Document);
  CheckEquals('Recebimento de Proventos', FOFCReader.Get(1).Description);
end;

procedure TTestOFCReader.TestMov4;
begin
  CheckEquals('D', FOFCReader.Get(4).MovType);
  CheckEquals(Double(EncodeDate(2016, 6, 3)), Double(FOFCReader.Get(4).MovDate), 0.0);
  CheckEquals('-200.00', FOFCReader.Get(4).Value);
  CheckEquals('20160603149980', FOFCReader.Get(4).ID);
  CheckEquals('00141658', FOFCReader.Get(4).Document);
  CheckEquals('Compra com Cart' + #227 + 'o - 03/06 11:34 LOJAS X', FOFCReader.Get(4).Description);
end;

initialization
  RegisterTest(TTestOFCReader);
end.
