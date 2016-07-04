
//
// OFX - Open Financial Exchange
// OFC - Open Financial Connectivity

// 2006 - Eduardo Bento da Rocha (YoungArts)
// 2016 - Leonardo Gregianin - github.com/leogregianin

unit ofxreader;

interface

uses classes, SysUtils;

type
  TOFXItem = class
    MovType : String;
    MovDate : TDate;
    Value : String;
    ID : string;
    Document : string;
    Description : string;
  end;

  TOFXReader = class(TComponent)
  public
    BankID : String;
    BranchID : string;
    AccountID : string;
    AccountType : string;
    FinalBalance : String;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function Import: boolean;
    function Get(iIndex: integer): TOFXItem;
    function Count: integer;
  private
    FOFXFile : string;
    FListItems : TList;
    procedure Clear;
    procedure Delete( iIndex: integer );
    function Add: TOFXItem;
    function PrepareFloat( sString : string ) : string;
    function InfLine ( sLine : string ): string;
    function FindString ( sSubString, sString : string ): boolean;
    function ReplaceString(sString: string; sOld: string; sNew: string; bInsensitive : boolean = true): string;
  protected
  published
    property OFXFile: string read FOFXFile write FOFXFile;
  end;

procedure Register;

implementation

constructor TOFXReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListItems := TList.Create;
end;

destructor TOFXReader.Destroy;
begin
  FListItems.Free;
  inherited Destroy;
end;

procedure TOFXReader.Delete( iIndex: integer );
begin
  TOFXItem(FListItems.Items[iIndex]).Free;
  FListItems.Delete( iIndex );
end;

procedure TOFXReader.Clear;
var
  i: integer;
  oPointer : Pointer;
begin
  while FListItems.Count > 0 do
    Delete(0);
  FListItems.Clear;
end;

function TOFXReader.Count: integer;
begin
  Result := FListItems.Count;
end;

function TOFXReader.Get(iIndex: integer): TOFXItem;
begin
  Result := TOFXItem(FListItems.Items[iIndex]);
end;

function TOFXReader.Import: boolean;
var
  oFile : TStringList;
  i : integer;
  bOFX : boolean;
  oItem : TOFXItem;
  sLine : string;
begin
  Result := false;
  Clear;
  bOFX := false;
  if not FileExists(FOFXFile) then
    exit;
  oFile := TStringList.Create;
  oFile.LoadFromFile(FOFXFile);
  i := 0;

  while i < oFile.Count do
  begin
    sLine := oFile.Strings[i];
    if FindString('<OFX>', sLine) or FindString('<OFC>', sLine) then
      bOFX := true;

    if bOFX then
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
      if FindString('<LEDGER>', sLine) or FindString('<BALAMT>', sLine) then
         FinalBalance := InfLine(sLine);

      // Movement
      if FindString('<STMTTRN>', sLine) then
      begin
        oItem := Add;
        while not FindString('</STMTTRN>', sLine) do
        begin
          Inc(i);
          sLine := oFile.Strings[i];

          if FindString('<TRNTYPE>', sLine) then
          begin
             if (InfLine(sLine) = '0') or (InfLine(sLine) = 'CREDIT') then
                oItem.MovType := 'C'
             else
             if (InfLine(sLine) = '1') or (InfLine(sLine) = 'DEBIT') then
                oItem.MovType := 'D'
             else oItem.MovType := 'OTHER';
          end;

          if FindString('<DTPOSTED>', sLine) then
             oItem.MovDate := EncodeDate(StrToIntDef(copy(InfLine(sLine),1,4), 0),
                                         StrToIntDef(copy(InfLine(sLine),5,2), 0),
                                         StrToIntDef(copy(InfLine(sLine),7,2), 0));

          if FindString('<FITID>', sLine) then
             oItem.ID := InfLine(sLine);

          if FindString('<CHKNUM>', sLine) or FindString('<CHECKNUM>', sLine) then
             oItem.Document := InfLine(sLine);

          if FindString('<MEMO>', sLine) then
             oItem.Description := InfLine(sLine);

          if FindString('<TRNAMT>', sLine) then
             oItem.Value := InfLine(sLine);
        end;
      end;

    end;
    Inc(i);
  end;
  Result := bOFX;
end;

function TOFXReader.PrepareFloat( sString : string ) : string;
begin
  Result := sString;
  Result := ReplaceString(Result, '.', DecimalSeparator);
  Result := ReplaceString(Result, ',', DecimalSeparator);
end;

function TOFXReader.ReplaceString(sString: string; sOld: string; sNew: string; bInsensitive : boolean = true): string;
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

function TOFXReader.InfLine ( sLine : string ): string;
var
  iTemp : integer;
begin
  Result := '';
  sLine := Trim(sLine);
  if FindString('>', sLine) then
  begin
    sLine := Trim(sLine);
    iTemp := Pos('>', sLine);
    if pos('</', sLine) > 0 then
       Result := copy(sLine, iTemp+1, pos('</', sLine)-iTemp-1)
    else
       Result := copy(sLine, iTemp+1, iTemp-1);
  end;
end;

function TOFXReader.Add: TOFXItem;
var
  oItem : TOFXItem;
begin
  oItem := TOFXItem.Create;
  FListItems.Add(oItem);
  Result := oItem;
end;

function TOFXReader.FindString ( sSubString, sString : string ): boolean;
begin
  Result := Pos(UpperCase(sSubString), UpperCase(sString)) > 0;
end;

procedure Register;
begin
  RegisterComponents('OFXReader', [TOFXReader]);
end;

end.
