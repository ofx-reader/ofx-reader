//
// OFX - Open Financial Exchange
// OFC - Open Financial Connectivity

// 2006 - Eduardo Bento da Rocha (YoungArts)
// 2016 - Leonardo Gregianin - github.com/leogregianin
// 2025 - Marlon Nardi - github.com/marlonnardi

unit ofxreader;

interface

uses Classes, SysUtils, DateUtils;

type
  TOFXItem = class
    MovType: String;
    MovDate: TDateTime;
    Value: String;
    ID: string;
    RefNum: string;
    Document: string;
    Description: string;
    Name: string;
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
    function InfLine(sLine: string): string;
    function FindString(sSubString, sString: string): boolean;
    function ConvertDate(DataStr: string): TDateTime;
  private
    FOFXFile: string;
    FOFXContent: string;
    FListItems: TList;
    procedure Clear;
    procedure Delete(iIndex: integer);
    function Add: TOFXItem;
  protected
  published
    property OFXFile: string read FOFXFile write FOFXFile;
    property OFXContent: string read FOFXContent write FOFXContent;
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

function TOFXReader.ConvertDate(DataStr: string): TDateTime;
var
  FS: TFormatSettings;
  Day, Month, Year: string;
begin
  if Length(DataStr) < 8 then
    Exit(0);

  FS := TFormatSettings.Create('pt-BR');

  try
    Day := Copy(DataStr, 1, 2);
    Month := Copy(DataStr, 3, 2);
    Year := Copy(DataStr, 5, 4);

    Result := StrToDate(Format('%s/%s/%s', [Day, Month, Year]), FS);
  except
    Result := 0;
  end;
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
  if (FOFXContent = '') and (not FileExists(FOFXFile)) then
    raise Exception.Create('File not found!');
  oFile := TStringList.Create;
  try
    if (FOFXContent = '') then
    begin
      try
        oFile.LoadFromFile(FOFXFile, TEncoding.UTF8);
      except
        oFile.LoadFromFile(FOFXFile);
      end;
    end
    else
      oFile.Add(FOFXContent);
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
          begin
            try
              DateStart :=
                DateToStr(EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                StrToIntDef(Copy(InfLine(sLine), 5, 2), 0),
                StrToIntDef(Copy(InfLine(sLine), 7, 2), 0)));
            except
              DateStart := DateToStr(ConvertDate(InfLine(sLine)));
            end;
          end;
        end;
        if FindString('<DTEND>', sLine) then
        begin
          if Trim(sLine) <> '' then
          begin
            try
              DateEnd :=
                DateToStr(EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                StrToIntDef(Copy(InfLine(sLine), 5, 2), 0),
                StrToIntDef(Copy(InfLine(sLine), 7, 2), 0)));
            except
              DateEnd := DateToStr(ConvertDate(InfLine(sLine)));
            end;
          end;
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
              begin
                try
                  oItem.MovDate :=
                    EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                    StrToIntDef(Copy(InfLine(sLine), 5, 2), 0),
                    StrToIntDef(Copy(InfLine(sLine), 7, 2), 0));
                except
                  oItem.MovDate := ConvertDate(InfLine(sLine));
                end;
              end;

            if (BankID <> '') and (StrToInt(BankID) = 341) and (oItem.MovDate = 0) and FindString('<FITID>', sLine)  then
            begin
              try
                oItem.MovDate :=
                    EncodeDate(StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                    StrToIntDef(Copy(InfLine(sLine), 5, 2), 0),
                    StrToIntDef(Copy(InfLine(sLine), 7, 2), 0));
              except
                oItem.MovDate := ConvertDate(InfLine(sLine));
              end;
            end;

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

            if FindString('<NAME>', sLine) then
              oItem.Name := InfLine(sLine);
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
      Result := Copy(sLine, iTemp + 1, Pos('</', sLine) - iTemp - 1)
    else
      Result := Copy(sLine, iTemp + 1, length(sLine));
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
  if sSubString = '' then
    Exit(True);

  Result := Pos(UpperCase(sSubString), UpperCase(sString)) > 0;
end;

procedure Register;
begin
  RegisterComponents('OFXReader', [TOFXReader]);
end;

end.
