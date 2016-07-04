unit testofcreader;

interface

uses
  TestFramework, classes, SysUtils, ofcreader, Controls;

type
  // Test methods for class TOFCReader
  TestTOFCReader = class(TTestCase)
  strict private
    FOFCReader: TOFCReader;
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

procedure TestTOFCReader.SetUp;
var
  ReturnValue: Boolean;
begin
  FOFCReader := TOFCReader.Create(nil);
  FOFCReader.OFCFile := 'extrato.ofc';
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
        CheckEquals('01/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('D', FOFCReader.Get(i).MovType);
        CheckEquals('-10.00', FOFCReader.Get(i).Value);
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
        CheckEquals('02/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('C', FOFCReader.Get(i).MovType);
        CheckEquals('880.00', FOFCReader.Get(i).Value);
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
        CheckEquals('03/06/2016', DateToStr(FOFCReader.Get(i).MovDate));
        CheckEquals('D', FOFCReader.Get(i).MovType);
        CheckEquals('-200.00', FOFCReader.Get(i).Value);
        CheckEquals('Compra com Cartão - 03/06 11:34 LOJAS X', FOFCReader.Get(i).Description);
      end;
     end;
  end;
end;

initialization
  RegisterTest(TestTOFCReader.Suite);
end.

