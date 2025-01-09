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
    MovType: String;
    MovDate: TDateTime;
    Value: String;
    ID: string;
    RefNum: string;
    Document: string;
    Description: string;
  end;

  TOFXReader = class(TComponent)
  public
    BankID: String;
    BranchID: string;
    AccountID: string;
    AccountType: string;
    DateStart: string;
    DateEnd: string;
    FinalBalance: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Import: boolean;
    function Get(iIndex: integer): TOFXItem;
    function Count: integer;
  private
    FOFXFile: string;
    FListItems: TList;
    procedure Clear;
    procedure Delete(iIndex: integer);
    function Add: TOFXItem;
    function InfLine(sLine: string): string;
    function FindString(sSubString, sString: string): boolean;
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
  Clear;
  FListItems.Free;
  inherited Destroy;
end;

procedure TOFXReader.Delete(iIndex: integer);
begin
  TOFXItem(FListItems.Items[iIndex]).Free;
  FListItems.Delete(iIndex);
end;

procedure TOFXReader.Clear;
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
  oFile: TStringList;
  i: integer;
  bOFX: boolean;
  oItem: TOFXItem;
  sLine: string;
begin
  Clear;
  DateStart := '';
  DateEnd := '';
  bOFX := false;
  if not FileExists(FOFXFile) then
    raise Exception.Create('File not found!');
  oFile := TStringList.Create;
  try
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
        if FindString('<BANKID>', sLine) then
          BankID := InfLine(sLine);

        // Agency
        if FindString('<BRANCHID>', sLine) then
          BranchID := InfLine(sLine);

        // Account
        if FindString('<ACCTID>', sLine) then
          AccountID := InfLine(sLine);

        // Account type
        if FindString('<ACCTTYPE>', sLine) then
          AccountType := InfLine(sLine);

        // Date Start and Date End
        if FindString('<DTSTART>', sLine) then
        begin
          if Trim(sLine) <> '' then
            DateStart :=
              DateToStr(EncodeDate(StrToIntDef(copy(InfLine(sLine), 1, 4), 0),
              StrToIntDef(copy(InfLine(sLine), 5, 2), 0),
              StrToIntDef(copy(InfLine(sLine), 7, 2), 0)));
        end;
        if FindString('<DTEND>', sLine) then
        begin
          if Trim(sLine) <> '' then
            DateEnd :=
              DateToStr(EncodeDate(StrToIntDef(copy(InfLine(sLine), 1, 4), 0),
              StrToIntDef(copy(InfLine(sLine), 5, 2), 0),
              StrToIntDef(copy(InfLine(sLine), 7, 2), 0)));
        end;

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
              if (InfLine(sLine) = '0') or (InfLine(sLine) = 'CREDIT') or (InfLine(sLine) = 'CREDITO') or (InfLine(sLine) = 'DEP') then
                oItem.MovType := 'C'
              else
              if (InfLine(sLine) = '1') or (InfLine(sLine) = 'DEBIT') or (InfLine(sLine) = 'DEBITO') or (InfLine(sLine) = 'XFER') then
                oItem.MovType := 'D'
              else
                oItem.MovType := 'OTHER';
            end;

            if FindString('<DTPOSTED>', sLine) then
              if Copy(InfLine(sLine), 1, 4) <> '' then
                oItem.MovDate :=
                  EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                  StrToIntDef(copy(InfLine(sLine), 5, 2), 0),
                  StrToIntDef(copy(InfLine(sLine), 7, 2), 0));

            if (StrToInt(BankID) = 341) and (oItem.MovDate = 0) and FindString('<FITID>', sLine)  then
              oItem.MovDate :=
                  EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                  StrToIntDef(copy(InfLine(sLine), 5, 2), 0),
                  StrToIntDef(copy(InfLine(sLine), 7, 2), 0));

            if FindString('<FITID>', sLine) then
              oItem.ID := InfLine(sLine);

            if FindString('<REFNUM>', sLine) then
              oItem.RefNum := InfLine(sLine);

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
  finally
    oFile.Free;
  end;
end;

function TOFXReader.InfLine(sLine: string): string;
var
  iTemp: integer;
begin
  Result := '';
  sLine := Trim(sLine);
  if FindString('>', sLine) then
  begin
    sLine := Trim(sLine);
    iTemp := Pos('>', sLine);
    if Pos('</', sLine) > 0 then
      Result := copy(sLine, iTemp + 1, Pos('</', sLine) - iTemp - 1)
    else
      // allows you to read the whole line when there is no completion of </ on the same line
      // made by weberdepaula@gmail.com
      Result := copy(sLine, iTemp + 1, length(sLine));
  end;
end;

function TOFXReader.Add: TOFXItem;
var
  oItem: TOFXItem;
begin
  oItem := TOFXItem.Create;
  FListItems.Add(oItem);
  Result := oItem;
end;

function TOFXReader.FindString(sSubString, sString: string): boolean;
begin
  Result := Pos(UpperCase(sSubString), UpperCase(sString)) > 0;
end;

procedure Register;
begin
  RegisterComponents('OFXReader', [TOFXReader]);
end;

end.
