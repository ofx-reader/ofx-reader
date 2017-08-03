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
    procedure TestHookUp;
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

procedure TTestCase1.TestHookUp;
begin
  Fail('Escreva seu próprio teste');
end;

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

procedure TTestCase1.TestBank;
begin
  CheckEquals('1', FOFXReader.BankID);
end;

procedure TTestCase1.TestBranch;
begin
  CheckEquals('1234-1', FOFXReader.BranchID);
end;

procedure TTestCase1.TestAccount;
begin
  CheckEquals('54321-9', FOFXReader.AccountID);
end;

procedure TTestCase1.TestAccountType;
begin
  CheckEquals('CHECKING', FOFXReader.AccountType);
end;

procedure TTestCase1.TestFinalBalance;
begin
  CheckEquals('-10.00', FOFXReader.FinalBalance);
end;

procedure TTestCase1.TestMov0;
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

procedure TTestCase1.TestMov1;
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

procedure TTestCase1.TestMov4;
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

initialization
  RegisterTest('TTestCase1.TestAccount', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestImport', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestBank', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestBranch', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestAccount', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestAccountType', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestFinalBalance', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestMov0', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestMov1', TTestCase1.Suite);
  RegisterTest('TTestCase1.TestMov4', TTestCase1.Suite);
end.

