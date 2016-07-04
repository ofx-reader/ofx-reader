unit Example1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormExample1 = class(TForm)
    ButtonReader: TButton;
    Memo1: TMemo;
    Path: TLabeledEdit;
    procedure ButtonReaderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormExample1: TFormExample1;

implementation

{$R *.dfm}

uses ofxreader;

procedure TFormExample1.ButtonReaderClick(Sender: TObject);
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
