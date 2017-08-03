unit uSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ofxreader;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonImport: TButton;
    Path: TEdit;
    Memo1: TMemo;
    procedure ButtonImportClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonImportClick(Sender: TObject);
var OFXReader1: TOFXReader;
    i: Integer;
begin
  try
     OFXReader1 := TOFXReader.Create(nil);

     OFXReader1.OFXFile := Path.Text;
     try
        OFXReader1.Import;
     except on E: Exception do
        raise Exception.Create('Error Message: ' + E.Message);
     end;

     Memo1.Clear;
     Memo1.Lines.Add('Bank: ' + OFXReader1.BankID);
     Memo1.Lines.Add('Branch: ' + OFXReader1.BranchID);
     Memo1.Lines.Add('Account: ' + OFXReader1.AccountID);
     Memo1.Lines.Add('----------------');
     Memo1.Lines.Add('# | '+
                     'Transaction identify | ' +
                     'Document | ' +
                     'Date | ' +
                     'Type | '+
                     'Value | '+
                     'Description ');
     Memo1.Lines.Add('----------------');

     for i := 0 to OFXReader1.Count-1 do
     begin
       Memo1.Lines.Add(IntToStr(i) + ' ' +
                       OFXReader1.Get(i).ID + ' ' +
                       OFXReader1.Get(i).Document + ' ' +
                       DateToStr(OFXReader1.Get(i).MovDate) + ' ' +
                       OFXReader1.Get(i).MovType + ' ' +
                       OFXReader1.Get(i).Value + ' ' +
                       OFXReader1.Get(i).Description);
     end;

     Memo1.Lines.Add('----------------');
     Memo1.Lines.Add('Final balance: ' + OFXReader1.FinalBalance);

  finally
     FreeAndNil(OFXReader1);
  end;
end;

end.

