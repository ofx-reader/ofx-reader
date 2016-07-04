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

uses ofcreader;

procedure TFormExample1.ButtonReaderClick(Sender: TObject);
var OFCReader1: TOFCReader;
    i: Integer;
begin
   try
      OFCReader1 := TOFCReader.Create(nil);

      OFCReader1.OFCFile := Path.Text;
      OFCReader1.Import;

      Memo1.Clear;
      Memo1.Lines.Add('Bank: ' + OFCReader1.BankID);
      Memo1.Lines.Add('Branch: ' + OFCReader1.BranchID);
      Memo1.Lines.Add('Account: ' + OFCReader1.AccountID);
      Memo1.Lines.Add('----------------');

      for i := 0 to OFCReader1.Count-1 do
      begin
        Memo1.Lines.Add(IntToStr(i) + ' ' +
                        OFCReader1.Get(i).ID + ' ' +
                        OFCReader1.Get(i).Document + ' ' +
                        DateToStr(OFCReader1.Get(i).MovDate) + ' ' +
                        OFCReader1.Get(i).MovType + ' ' +
                        OFCReader1.Get(i).Value + ' ' +
                        OFCReader1.Get(i).Description);
      end;

      Memo1.Lines.Add('----------------');
      Memo1.Lines.Add('Final balance: ' + OFCReader1.FinalBalance);

   finally
      FreeAndNil(OFCReader1);
   end;
end;

end.
