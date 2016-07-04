
//
// OFC - Open Financial Connectivity
// 2006 - Eduardo Bento da Rocha (YoungArts)
// 2016 - Leonardo Gregianin - github.com/leogregianin

unit ofcreader;

interface

uses classes, SysUtils, Controls;

type
  TOFCItem = class
    MovType : String;
    MovDate : TDate;
    Value : String;
    ID : string;
    Document : string;
    Description : string;
  end;

  TOFCReader = class(TComponent)
  public
    BankID : String;
    BranchID : string;
    AccountID : string;
    AccountType : string;
    FinalBalance : String;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function Import: boolean;
    function Get(iIndex: integer): TOFCItem;
    function Count: integer;
  private
    FOFCFile : string;
    FListItems : TList;
    procedure Clear;
    procedure Delete( iIndex: integer );
    function Add: TOFCItem;
    function PrepareFloat( sString : string ) : string;
    function InfLine ( sLine : string ): string;
    function FindString ( sSubString, sString : string ): boolean;
    function ReplaceString(sString: string; sOld: string; sNew: string; bInsensitive : boolean = true): string;
  protected
  published
    property OFCFile: string read FOFCFile write FOFCFile;
  end;

procedure Register;

implementation

constructor TOFCReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListItems := TList.Create;
end;

destructor TOFCReader.Destroy;
begin
  FListItems.Free;
  inherited Destroy;
end;

procedure TOFCReader.Delete( iIndex: integer );
begin
  TOFCItem(FListItems.Items[iIndex]).Free;
  FListItems.Delete( iIndex );
end;

procedure TOFCReader.Clear;
var
  i: integer;
  oPointer : Pointer;
begin
  while FListItems.Count > 0 do
    Delete(0);
  FListItems.Clear;
end;

function TOFCReader.Count: integer;
begin
  Result := FListItems.Count;
end;

function TOFCReader.Get(iIndex: integer): TOFCItem;
begin
  Result := TOFCItem(FListItems.Items[iIndex]);
end;

function TOFCReader.Import: boolean;
var
  oFile : TStringList;
  i : integer;
  bOfc : boolean;
  oItem : TOFCItem;
  sLine : string;
begin
  Result := false;
  Clear;
  bOfc := false;
  if not FileExists(FOFCFile) then
    exit;
  oFile := TStringList.Create;
  oFile.LoadFromFile(FOFCFile);
  i := 0;

  while i < oFile.Count do
  begin
    sLine := oFile.Strings[i];
    if FindString('<OFC>', sLine) then
      bOfc := true;
    if bOfc then
    begin

      // Bank
      if FindString('<BANKID>', sLine) then BankID := InfLine(sLine);

      // Agency
      if FindString('<BRANCHID>', sLine) then BranchID := InfLine(sLine);

      // Account
      if FindString('<ACCTID>', sLine) then AccountID := InfLine(sLine);

      // Account type
      if FindString('<ACCTTYPE>', sLine) then AccountType := InfLine(sLine);

      // Final
      if FindString('<LEDGER>', sLine) then FinalBalance := InfLine(sLine);

      // Moviment
      if FindString('<STMTTRN>', sLine) then
      begin
        oItem := Add;
        while not FindString('</STMTTRN>', sLine) do
        begin
          Inc(i);
          sLine := oFile.Strings[i];
          if FindString('<TRNTYPE>',  sLine) then
             if InfLine(sLine) = '0' then oItem.MovType := 'C'
             else if InfLine(sLine) = '1' then oItem.MovType := 'D';

          if FindString('<DTPOSTED>', sLine) then oItem.MovDate := EncodeDate(StrToIntDef(copy(InfLine(sLine),1,4), 0),
                                                                              StrToIntDef(copy(InfLine(sLine),5,2), 0),
                                                                              StrToIntDef(copy(InfLine(sLine),7,2), 0));
          if FindString('<FITID>',    sLine) then oItem.ID          := InfLine(sLine);
          if FindString('<CHKNUM>',   sLine) then oItem.Document    := InfLine(sLine);
          if FindString('<MEMO>',     sLine) then oItem.Description := InfLine(sLine);
          if FindString('<TRNAMT>',   sLine) then oItem.Value       := InfLine(sLine);
        end;
      end;

    end;
    Inc(i);
  end;
  Result := bOfc;
end;

function TOFCReader.PrepareFloat( sString : string ) : string;
begin
  Result := sString;
  Result := ReplaceString(Result, '.', DecimalSeparator);
  Result := ReplaceString(Result, ',', DecimalSeparator);
end;

function TOFCReader.ReplaceString(sString: string; sOld: string; sNew: string; bInsensitive : boolean = true): string;
var
   iPosition: integer ;
   sTempNew: string ;
begin
   iPosition := 1;
   sTempNew := '';
   while (iPosition > 0) do
   begin
      if bInsensitive then
        iPosition := AnsiPos(UpperCase(sOld),UpperCase(sString))
      else
        iPosition := AnsiPos(sOld,sString);
      if (iPosition > 0) then
      begin
         sTempNew := sTempNew + copy(sString, 1, iPosition - 1) + sNew;
         sString := copy(sString, iPosition + Length(sOld), Length(sString) );
      end;
   end;
   sTempNew := sTempNew + sString;
   Result := (sTempNew);
end;

function TOFCReader.InfLine ( sLine : string ): string;
var
  iTemp : integer;
begin
  Result := '';
  sLine := Trim(sLine);
  if FindString('>', sLine) then
  begin
    sLine := Trim(sLine);
    iTemp := Pos('>', sLine);
    Result := copy(sLine, iTemp+1, pos('</', sLine)-iTemp-1)
  end;
end;

function TOFCReader.Add: TOFCItem;
var
  oItem : TOFCItem;
begin
  oItem := TOFCItem.Create;
  FListItems.Add(oItem);
  Result := oItem;
end;

function TOFCReader.FindString ( sSubString, sString : string ): boolean;
begin
  Result := Pos(UpperCase(sSubString), UpperCase(sString)) > 0;
end;

procedure Register;
begin
  RegisterComponents('ofcreader', [TOFCReader]);
end;

end.
