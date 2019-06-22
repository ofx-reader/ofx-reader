unit ExampleClientDataset;

interface

uses
  {$IfDef VER210}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBClient, Grids, DBGrids, StdCtrls, ExtCtrls;
  {$Else}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls, DB, DBClient, Controls, Grids,
  DBGrids, StdCtrls, Classes, ExtCtrls;
  {$EndIf}

type
  TForm1 = class(TForm)
    Path: TLabeledEdit;
    Button1: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ofxreader;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  tmpReader: TOFXReader;
  i: Integer;
begin
  tmpReader := TOFXReader.Create(Self);
  try
    tmpReader.OFXFile := ExpandFileName(Path.Text);
    if not (tmpReader.Import) then
      raise Exception.Create(Format('Import file %s with errors!', [tmpReader.OFXFile]));

    for i := 0 to tmpReader.Count - 1 do
      ClientDataSet1.InsertRecord([
        i,
        tmpReader.Get(i).ID,
        tmpReader.Get(i).Document,
        tmpReader.Get(i).MovDate,
        tmpReader.Get(i).MovType,
        tmpReader.Get(i).Value,
        tmpReader.Get(i).Description
      ]);
  finally
    tmpReader.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  STR_SIZE = 60;
begin
  ClientDataSet1.FieldDefs.Add('INDEX', ftString, STR_SIZE);
  ClientDataSet1.FieldDefs.Add('ID', ftString, STR_SIZE);
  ClientDataSet1.FieldDefs.Add('DOCUMENT', ftString, STR_SIZE);
  ClientDataSet1.FieldDefs.Add('MOVDATE', ftDate);
  ClientDataSet1.FieldDefs.Add('MOVTYPE', ftString, STR_SIZE);
  ClientDataSet1.FieldDefs.Add('VALUE', ftString, STR_SIZE);
  ClientDataSet1.FieldDefs.Add('DESCRIPTION', ftString, STR_SIZE);
  ClientDataSet1.CreateDataSet;
end;

end.
