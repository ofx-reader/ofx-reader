unit ExampleJSON;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.JSON;

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
    LJson, LJsonObject: TJSONObject;
    LArr: TJSONArray;
begin
   try
      OFXReader1 := TOFXReader.Create(nil);

      OFXReader1.OFXFile := Path.Text;
      try
         OFXReader1.Import;
      except on E: Exception do
         raise Exception.Create('Error Message: ' + E.Message);
      end;


      // Create JSON
      LJsonObject := TJSONObject.Create;
      LJsonObject.AddPair(TJSONPair.Create('Bank', OFXReader1.BankID));
      LJsonObject.AddPair(TJSONPair.Create('Branch', OFXReader1.BranchID));
      LJsonObject.AddPair(TJSONPair.Create('Account', OFXReader1.AccountID));

      for i := 0 to OFXReader1.Count-1 do
      begin
        LArr := TJSONArray.Create;
        LJson := TJSONObject.Create;
        LJson.AddPair(TJSONPair.Create('Index', IntToStr(i)));
        LJson.AddPair(TJSONPair.Create('Transaction', OFXReader1.Get(i).ID));
        LJson.AddPair(TJSONPair.Create('Document', OFXReader1.Get(i).Document));
        LJson.AddPair(TJSONPair.Create('Date', DateToStr(OFXReader1.Get(i).MovDate)));
        LJson.AddPair(TJSONPair.Create('Type', OFXReader1.Get(i).MovType));
        LJson.AddPair(TJSONPair.Create('Value', OFXReader1.Get(i).Value));
        LJson.AddPair(TJSONPair.Create('Description', OFXReader1.Get(i).Description));
        LArr.Add(LJson);
        LJsonObject.AddPair(TJSONPair.Create('Transactions', LArr));
      end;

      LJsonObject.AddPair(TJSONPair.Create('Final balance', OFXReader1.FinalBalance));

      Memo1.Clear;
      Memo1.Text := LJsonObject.ToString;

   finally
      LJsonObject.Free;
      FreeAndNil(OFXReader1);
   end;
end;

end.
