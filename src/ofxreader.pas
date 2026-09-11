//
// OFX - Open Financial Exchange
// OFC - Open Financial Connectivity

// 2006 - Eduardo Bento da Rocha (YoungArts)
// 2016 - Leonardo Gregianin - github.com/leogregianin
// 2025 - Marlon Nardi - github.com/marlonnardi

unit ofxreader;

interface

uses
  Classes, SysUtils, DateUtils;

{$WARN NO_RETVAL OFF}
type
  TOFXItem = class
    MovType: string;
    MovDate: TDateTime;
    Value: string;
    ID: string;
    RefNum: string;
    Document: string;
    Description: string;
    Name: string;
    SourceKey: string;
  end;

  TOFXReader = class(TComponent)
  public
    BankID: string;
    BranchID: string;
    AccountID: string;
    AccountType: string;
    DateStart: string;
    DateEnd: string;
    FinalBalance: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Import: Boolean;
    function Get(iIndex: integer): TOFXItem;
    function Count: integer;
    procedure FormatOFX(const InputFile, OutputFile: string);
  private
    FOFXFile: string;
    FOFXContent: string;
    FListItems: TList;
    function ImportCSV: Boolean;
    procedure Clear;
    procedure Reset;
    procedure Delete(iIndex: integer);
    function Add: TOFXItem;
    function InfLine(sLine: string): string;
    function FindString(sSubString, sString: string): Boolean;
    function ConvertDate(DataStr: string): TDateTime;
  protected
    function GetBetween(Str, StrStart, StrEnd: string): string;
  published
    property OFXFile: string read FOFXFile write FOFXFile;
    property OFXContent: string read FOFXContent write FOFXContent;
  end;

procedure Register;

implementation

{$IFNDEF FPC}
uses
  uExtratoCsvReader;
{$ENDIF}

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

procedure TOFXReader.Reset;
begin
  Clear;
  BankID := '';
  BranchID := '';
  AccountID := '';
  AccountType := '';
  DateStart := '';
  DateEnd := '';
  FinalBalance := '';
end;

function TOFXReader.ConvertDate(DataStr: string): TDateTime;
var
  FS: TFormatSettings;
begin

  FS := TFormatSettings.Create('pt-BR');
  try
    FS.ShortDateFormat := 'ddmmyyyy';
    Result := StrToDate(Copy(DataStr, 1, 8), FS);
  except
    //on e: Exception do
    //  raise Exception.Create('Erro ao converter a data: ' + DataStr + ' ' + e.Message);
  end;
end;

function TOFXReader.Count: integer;
begin
  Result := FListItems.Count;
end;

function TOFXReader.Get(iIndex: integer): TOFXItem;
begin
  Result := TOFXItem(FListItems.Items[iIndex]);
end;

function TOFXReader.GetBetween(Str, StrStart, StrEnd: string): string;
var
  iPosIni: Integer;
  iPosFim: Integer;
begin
  Result := '';
  iPosIni := Pos(StrStart, Str);

  if iPosIni <> 0 then
  begin
    System.Delete(Str, 1, iPosIni + Length(StrStart) - 1);
    iPosFim := Pos(StrEnd, Str);
    System.Delete(Str, iPosFim, Length(Str));
    Result := Str;
  end;
end;

function TOFXReader.Import: Boolean;
var
  oFile: TStringList;
  i: integer;
  bOFX: Boolean;
  oItem: TOFXItem;
  sLine: string;
begin
  Reset;
  bOFX := false;

  if (FOFXContent = '') and (not FileExists(FOFXFile)) then
    raise Exception.Create('File not found!');

  if (FOFXContent = '') and SameText(ExtractFileExt(FOFXFile), '.csv') then
    Exit(ImportCSV);

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
              DateStart := DateToStr(EncodeDate(
                StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
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
              DateEnd := DateToStr(EncodeDate(
                StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
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
              else if (InfLine(sLine) = '1') or (InfLine(sLine) = 'DEBIT') or (InfLine(sLine) = 'DEBITO') or (InfLine(sLine) = 'XFER') then
                oItem.MovType := 'D'
              else
                oItem.MovType := 'OTHER';
            end;

            if FindString('<DTPOSTED>', sLine) then
              if Copy(InfLine(sLine), 1, 4) <> '' then
              begin
                try
                  oItem.MovDate := EncodeDate(
                    StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
                    StrToIntDef(Copy(InfLine(sLine), 5, 2), 0),
                    StrToIntDef(Copy(InfLine(sLine), 7, 2), 0));
                except
                  oItem.MovDate := ConvertDate(InfLine(sLine));
                end;
              end;

            if (BankID <> '') and (StrToInt(BankID) = 341) and (oItem.MovDate = 0) and FindString('<FITID>', sLine) then
            begin
              try
                oItem.MovDate := EncodeDate(
                  StrToIntDef(Copy(InfLine(sLine), 1, 4), 0),
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

function TOFXReader.ImportCSV: Boolean;
{$IFDEF FPC}
begin
  raise Exception.Create('CSV import is not available in Lazarus/FPC builds.');
end;
{$ELSE}
var
  CSVReader: TExtratoCsvReader;
  I: Integer;
  Item: TOFXItem;
  Transacao: TExtratoCsvTransacao;
begin
  CSVReader := TExtratoCsvReader.Create;
  try
    CSVReader.CarregarArquivo(FOFXFile);
    DateStart := DateToStr(CSVReader.DataInicial);
    DateEnd := DateToStr(CSVReader.DataFinal);

    for I := 0 to CSVReader.Count - 1 do
    begin
      Transacao := CSVReader[I];
      Item := Add;
if Transacao.Tipo = 'DEBIT' then
        Item.MovType := 'D'
      else
        Item.MovType := 'C';
      Item.MovDate := Transacao.Data;
      Item.Value := CurrToStr(Transacao.Valor, TFormatSettings.Invariant);
      Item.ID := Transacao.Documento;
      Item.RefNum := Transacao.Documento;
      Item.Document := Transacao.Documento;
      Item.Description := Transacao.Descricao;
      Item.SourceKey := 'CSV|' + Transacao.ChaveOrigem;
    end;

    Result := Count > 0;
  finally
    CSVReader.Free;
  end;
end;
{$ENDIF}

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
      // allows you to read the whole line when there is no completion of </ on the same line
      // made by weberdepaula@gmail.com
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

function TOFXReader.FindString(sSubString, sString: string): Boolean;
begin
  Result := Pos(UpperCase(sSubString), UpperCase(sString)) > 0;
end;

procedure TOFXReader.FormatOFX(const InputFile, OutputFile: string);
var
  Input, Output: TStringList;
  i, Indent: Integer;
  Line, CurrentTag: string;

  function TrimTags(const S: string): string;
  begin
    Result := StringReplace(StringReplace(S, '<', '', []), '>', '', []);
  end;

begin
  Input := TStringList.Create;
  Output := TStringList.Create;
  try
    Input.LoadFromFile(InputFile);

    Indent := 0;

    for i := 0 to Input.Count - 1 do
    begin
      Line := Trim(Input[i]);

      if Line = '' then
        Continue;

      while Length(Line) > 0 do
      begin
        if Line[1] = '<' then
        begin
          if Pos('</', Line) = 1 then
            Dec(Indent);

          CurrentTag := Copy(Line, 1, Pos('>', Line));
          Line := Copy(Line, Pos('>', Line) + 1, MaxInt);

          if (Length(Line) > 0) and (Line[1] <> '<') then
          begin
            // Tag com valor curto fica na mesma linha
            Output.Add(StringOfChar(' ', Indent * 4) + CurrentTag + Trim(Copy(Line, 1, Pos('<', Line) - 1)) + Copy(Line, Pos('<', Line), Pos('>', Line) - Pos('<', Line)));
            Line := Copy(Line, Pos('>', Line) + 1, MaxInt);
          end
          else
            Output.Add(StringOfChar(' ', Indent * 4) + CurrentTag);

          if (Pos('</', CurrentTag) = 0) and (Pos('/>', CurrentTag) = 0) and (Line = '') then
            Inc(Indent);
        end
        else
        begin
          if Output.Count > 0 then
            Output[Output.Count - 1] := Output[Output.Count - 1] + Trim(Line)
          else
            Output.Add(StringOfChar(' ', Indent * 4) + Trim(Line));
          Line := '';
        end;
      end;
    end;

    Output.SaveToFile(OutputFile);
  finally
    Input.Free;
    Output.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('OFXReader', [TOFXReader]);
end;

end.
