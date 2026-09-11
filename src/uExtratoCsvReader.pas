unit uExtratoCsvReader;

interface

uses
  System.Classes, System.Generics.Collections, System.SysUtils;

type
  TExtratoCsvTransacao = record
    Data: TDate;
    Descricao: string;
    Documento: string;
    Valor: Currency;
    Tipo: string;
    ChaveOrigem: string;
    Linha: Integer;
  end;

  TExtratoCsvReader = class
  private
    FTransacoes: TList<TExtratoCsvTransacao>;
    FOcorrencias: TDictionary<string, Integer>;
    FDataInicial: TDate;
    FDataFinal: TDate;
    FCabecalhoProcessado: Boolean;
    FIndiceData: Integer;
    FIndiceDescricao: Integer;
    FIndiceDetalhes: Integer;
    FIndiceDocumento: Integer;
    FIndiceValor: Integer;
    FIndiceTipo: Integer;
    function GetCount: Integer;
    function GetTransacao(Index: Integer): TExtratoCsvTransacao;
    class function DetectarDelimitador(const Texto: string): Char; static;
    class function EhUtf8Valido(const Bytes: TBytes; Inicio: Integer): Boolean; static;
    class function LerArquivoTexto(const Arquivo: string): string; static;
    class function Normalizar(const Valor: string): string; static;
    class function TryLerData(const Valor: string; out Data: TDate): Boolean; static;
    class function TryLerValor(const Texto: string; out Valor: Currency): Boolean; static;
    class function ValorCampo(Campos: TStrings; Indice: Integer): string; static;
    class function EncontrarColuna(Cabecalhos: TStrings;
      const Nomes: array of string): Integer; static;
    class function EhLinhaEmBranco(Campos: TStrings): Boolean; static;
    class function EhLinhaSaldo(const Descricao: string): Boolean; static;
    procedure AnalisarTexto(const Texto: string; Delimitador: Char);
    procedure ProcessarLinha(Campos: TStrings; NumeroLinha: Integer);
    procedure ProcessarCabecalho(Campos: TStrings; NumeroLinha: Integer);
    procedure ProcessarTransacao(Campos: TStrings; NumeroLinha: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CarregarArquivo(const Arquivo: string);
    property Count: Integer read GetCount;
    property DataInicial: TDate read FDataInicial;
    property DataFinal: TDate read FDataFinal;
    property Transacoes[Index: Integer]: TExtratoCsvTransacao read GetTransacao; default;
  end;

implementation

uses
  System.IOUtils, System.Math;

constructor TExtratoCsvReader.Create;
begin
  inherited Create;
  FTransacoes := TList<TExtratoCsvTransacao>.Create;
  FOcorrencias := TDictionary<string, Integer>.Create;
end;

destructor TExtratoCsvReader.Destroy;
begin
  FOcorrencias.Free;
  FTransacoes.Free;
  inherited;
end;

procedure TExtratoCsvReader.CarregarArquivo(const Arquivo: string);
var
  Texto: string;
begin
  FTransacoes.Clear;
  FOcorrencias.Clear;
  FDataInicial := 0;
  FDataFinal := 0;
  FCabecalhoProcessado := False;
  FIndiceData := -1;
  FIndiceDescricao := -1;
  FIndiceDetalhes := -1;
  FIndiceDocumento := -1;
  FIndiceValor := -1;
  FIndiceTipo := -1;

  Texto := LerArquivoTexto(Arquivo);
  if Texto.Trim.IsEmpty then
    raise Exception.Create('O arquivo CSV está vazio.');

  AnalisarTexto(Texto, DetectarDelimitador(Texto));
  if not FCabecalhoProcessado then
    raise Exception.Create('Não foi possível localizar o cabeçalho do arquivo CSV.');
  if FTransacoes.Count = 0 then
    raise Exception.Create('Arquivo CSV importado sem movimentação bancária.');
end;

function TExtratoCsvReader.GetCount: Integer;
begin
  Result := FTransacoes.Count;
end;

function TExtratoCsvReader.GetTransacao(Index: Integer): TExtratoCsvTransacao;
begin
  Result := FTransacoes[Index];
end;

class function TExtratoCsvReader.EhUtf8Valido(const Bytes: TBytes;
  Inicio: Integer): Boolean;
var
  I, Tamanho: Integer;
  B: Byte;

  function Continuacao(Indice: Integer): Boolean;
  begin
    Result := (Indice < Tamanho) and ((Bytes[Indice] and $C0) = $80);
  end;

begin
  I := Inicio;
  Tamanho := Length(Bytes);
  while I < Tamanho do
  begin
    B := Bytes[I];
    if B <= $7F then
      Inc(I)
    else if (B >= $C2) and (B <= $DF) and Continuacao(I + 1) then
      Inc(I, 2)
    else if (B = $E0) and (I + 2 < Tamanho) and
      (Bytes[I + 1] >= $A0) and (Bytes[I + 1] <= $BF) and Continuacao(I + 2) then
      Inc(I, 3)
    else if (((B >= $E1) and (B <= $EC)) or ((B >= $EE) and (B <= $EF))) and
      Continuacao(I + 1) and Continuacao(I + 2) then
      Inc(I, 3)
    else if (B = $ED) and (I + 2 < Tamanho) and
      (Bytes[I + 1] >= $80) and (Bytes[I + 1] <= $9F) and Continuacao(I + 2) then
      Inc(I, 3)
    else if (B = $F0) and (I + 3 < Tamanho) and
      (Bytes[I + 1] >= $90) and (Bytes[I + 1] <= $BF) and
      Continuacao(I + 2) and Continuacao(I + 3) then
      Inc(I, 4)
    else if (B >= $F1) and (B <= $F3) and Continuacao(I + 1) and
      Continuacao(I + 2) and Continuacao(I + 3) then
      Inc(I, 4)
    else if (B = $F4) and (I + 3 < Tamanho) and
      (Bytes[I + 1] >= $80) and (Bytes[I + 1] <= $8F) and
      Continuacao(I + 2) and Continuacao(I + 3) then
      Inc(I, 4)
    else
      Exit(False);
  end;
  Result := True;
end;

class function TExtratoCsvReader.LerArquivoTexto(const Arquivo: string): string;
var
  Bytes: TBytes;
  Encoding: TEncoding;
begin
  Bytes := TFile.ReadAllBytes(Arquivo);
  if (Length(Bytes) >= 3) and (Bytes[0] = $EF) and (Bytes[1] = $BB) and
    (Bytes[2] = $BF) then
    Exit(TEncoding.UTF8.GetString(Bytes, 3, Length(Bytes) - 3));

  if (Length(Bytes) >= 2) and (Bytes[0] = $FF) and (Bytes[1] = $FE) then
    Exit(TEncoding.Unicode.GetString(Bytes, 2, Length(Bytes) - 2));

  if (Length(Bytes) >= 2) and (Bytes[0] = $FE) and (Bytes[1] = $FF) then
    Exit(TEncoding.BigEndianUnicode.GetString(Bytes, 2, Length(Bytes) - 2));

  if EhUtf8Valido(Bytes, 0) then
    Exit(TEncoding.UTF8.GetString(Bytes));

  Encoding := TEncoding.GetEncoding(1252);
  try
    Result := Encoding.GetString(Bytes);
  finally
    Encoding.Free;
  end;
end;

class function TExtratoCsvReader.DetectarDelimitador(const Texto: string): Char;
var
  I, Virgulas, PontoVirgulas, Tabulacoes: Integer;
  EntreAspas: Boolean;
begin
  if Texto.StartsWith('sep=', True) and (Texto.Length >= 5) then
    Exit(Texto[5]);

  Virgulas := 0;
  PontoVirgulas := 0;
  Tabulacoes := 0;
  EntreAspas := False;
  I := 1;
  while I <= Texto.Length do
  begin
    if Texto[I] = '"' then
    begin
      if EntreAspas and (I < Texto.Length) and (Texto[I + 1] = '"') then
        Inc(I)
      else
        EntreAspas := not EntreAspas;
    end
    else if not EntreAspas then
    begin
      case Texto[I] of
        ',': Inc(Virgulas);
        ';': Inc(PontoVirgulas);
        #9: Inc(Tabulacoes);
        #10, #13: Break;
      end;
    end;
    Inc(I);
  end;

  if (Virgulas = 0) and (PontoVirgulas = 0) and (Tabulacoes = 0) then
    raise Exception.Create('Não foi possível identificar o separador do arquivo CSV.');
  if (PontoVirgulas >= Virgulas) and (PontoVirgulas >= Tabulacoes) then
    Result := ';'
  else if Tabulacoes >= Virgulas then
    Result := #9
  else
    Result := ',';
end;

procedure TExtratoCsvReader.AnalisarTexto(const Texto: string;
  Delimitador: Char);
var
  Campo: TStringBuilder;
  Campos: TStringList;
  I, LinhaAtual, LinhaRegistro: Integer;
  EntreAspas: Boolean;
  C: Char;

  procedure FinalizarCampo;
  begin
    Campos.Add(Campo.ToString);
    Campo.Clear;
  end;

  procedure FinalizarLinha;
  begin
    FinalizarCampo;
    ProcessarLinha(Campos, LinhaRegistro);
    Campos.Clear;
    LinhaRegistro := LinhaAtual + 1;
  end;

begin
  Campo := TStringBuilder.Create;
  Campos := TStringList.Create;
  try
    EntreAspas := False;
    LinhaAtual := 1;
    LinhaRegistro := 1;
    I := 1;
    while I <= Texto.Length do
    begin
      C := Texto[I];
      if EntreAspas then
      begin
        if C = '"' then
        begin
          if (I < Texto.Length) and (Texto[I + 1] = '"') then
          begin
            Campo.Append('"');
            Inc(I);
          end
          else
            EntreAspas := False;
        end
        else
        begin
          Campo.Append(C);
          if C = #10 then
            Inc(LinhaAtual);
        end;
      end
      else if (C = '"') and (Campo.Length = 0) then
        EntreAspas := True
      else if C = Delimitador then
        FinalizarCampo
      else if (C = #10) or (C = #13) then
      begin
        FinalizarLinha;
        if (C = #13) and (I < Texto.Length) and (Texto[I + 1] = #10) then
          Inc(I);
        Inc(LinhaAtual);
      end
      else
        Campo.Append(C);
      Inc(I);
    end;

    if EntreAspas then
      raise Exception.CreateFmt('Aspas não finalizadas no arquivo CSV, a partir da linha %d.',
        [LinhaRegistro]);
    if (Campo.Length > 0) or (Campos.Count > 0) then
      FinalizarLinha;
  finally
    Campos.Free;
    Campo.Free;
  end;
end;

procedure TExtratoCsvReader.ProcessarLinha(Campos: TStrings;
  NumeroLinha: Integer);
begin
  if EhLinhaEmBranco(Campos) then
    Exit;
  if (not FCabecalhoProcessado) and (Campos.Count = 1) and
    Campos[0].Trim.StartsWith('sep=', True) then
    Exit;
  if not FCabecalhoProcessado then
    ProcessarCabecalho(Campos, NumeroLinha)
  else
    ProcessarTransacao(Campos, NumeroLinha);
end;

procedure TExtratoCsvReader.ProcessarCabecalho(Campos: TStrings;
  NumeroLinha: Integer);
var
  Ausentes: string;
begin
  FIndiceData := EncontrarColuna(Campos,
    ['DATA', 'DATA MOVIMENTO', 'DATA LANCAMENTO', 'DATE']);
  FIndiceDescricao := EncontrarColuna(Campos,
    ['LANCAMENTO', 'HISTORICO', 'DESCRICAO', 'DESCRICAO DETALHADA', 'MEMO']);
  FIndiceDetalhes := EncontrarColuna(Campos,
    ['DETALHES', 'COMPLEMENTO', 'DESCRICAO DETALHADA']);
  FIndiceDocumento := EncontrarColuna(Campos,
    ['N DOCUMENTO', 'NUMERO DOCUMENTO', 'DOCUMENTO', 'DOC',
     'ID TRANSACAO', 'IDENTIFICADOR']);
  FIndiceValor := EncontrarColuna(Campos,
    ['VALOR', 'VALOR LANCAMENTO', 'AMOUNT']);
  FIndiceTipo := EncontrarColuna(Campos,
    ['TIPO LANCAMENTO', 'TIPO', 'NATUREZA', 'DEBITO CREDITO',
     'ENTRADA SAIDA']);

  Ausentes := '';
  if FIndiceData < 0 then
    Ausentes := 'Data';
  if FIndiceDescricao < 0 then
  begin
    if Ausentes <> '' then
      Ausentes := Ausentes + ', ';
    Ausentes := Ausentes + 'Lançamento/Descrição';
  end;
  if FIndiceValor < 0 then
  begin
    if Ausentes <> '' then
      Ausentes := Ausentes + ', ';
    Ausentes := Ausentes + 'Valor';
  end;
  if Ausentes <> '' then
    raise Exception.CreateFmt('Cabeçalho CSV incompatível na linha %d. Colunas não localizadas: %s.',
      [NumeroLinha, Ausentes]);
  FCabecalhoProcessado := True;
end;

procedure TExtratoCsvReader.ProcessarTransacao(Campos: TStrings;
  NumeroLinha: Integer);
var
  Transacao: TExtratoCsvTransacao;
  DataTexto, Descricao, Detalhes, TipoOriginal, TipoNormalizado: string;
  BaseChave: string;
  Ocorrencia: Integer;
  Valor: Currency;
begin
  Descricao := ValorCampo(Campos, FIndiceDescricao).Trim;
  Detalhes := ValorCampo(Campos, FIndiceDetalhes).Trim;
  if EhLinhaSaldo(Descricao) or EhLinhaSaldo(Detalhes) then
    Exit;

  DataTexto := ValorCampo(Campos, FIndiceData).Trim;
  if not TryLerData(DataTexto, Transacao.Data) then
    raise Exception.CreateFmt('Data inválida na linha %d do arquivo CSV.', [NumeroLinha]);

  if not TryLerValor(ValorCampo(Campos, FIndiceValor), Valor) then
    raise Exception.CreateFmt('Valor inválido na linha %d do arquivo CSV.', [NumeroLinha]);

  TipoOriginal := ValorCampo(Campos, FIndiceTipo).Trim;
  TipoNormalizado := Normalizar(TipoOriginal);
  if (TipoNormalizado = 'SAIDA') or (TipoNormalizado = 'DEBITO') or
    (TipoNormalizado = 'DEBIT') or (TipoNormalizado = 'D') then
  begin
    Valor := -Abs(Valor);
    Transacao.Tipo := 'DEBIT';
  end
  else if (TipoNormalizado = 'ENTRADA') or (TipoNormalizado = 'CREDITO') or
    (TipoNormalizado = 'CREDIT') or (TipoNormalizado = 'C') then
  begin
    Valor := Abs(Valor);
    Transacao.Tipo := 'CREDIT';
  end
  else if Valor < 0 then
    Transacao.Tipo := 'DEBIT'
  else
    Transacao.Tipo := 'CREDIT';

  if (Descricao = '') and (Detalhes = '') then
    raise Exception.CreateFmt('Descrição não informada na linha %d do arquivo CSV.',
      [NumeroLinha]);
  if (Detalhes <> '') and not SameText(Descricao, Detalhes) then
  begin
    if Descricao <> '' then
      Descricao := Descricao + ' | ' + Detalhes
    else
      Descricao := Detalhes;
  end;

  Transacao.Descricao := Descricao;
  Transacao.Documento := ValorCampo(Campos, FIndiceDocumento).Trim;
  Transacao.Valor := Valor;
  Transacao.Linha := NumeroLinha;

  BaseChave := FormatDateTime('yyyymmdd', Transacao.Data) + '|' +
    Transacao.Documento + '|' +
    CurrToStr(Transacao.Valor, TFormatSettings.Invariant) + '|' +
    Transacao.Descricao;
  if not FOcorrencias.TryGetValue(BaseChave, Ocorrencia) then
    Ocorrencia := 0;
  Inc(Ocorrencia);
  FOcorrencias.AddOrSetValue(BaseChave, Ocorrencia);
  Transacao.ChaveOrigem := BaseChave + '|' + Ocorrencia.ToString;

  FTransacoes.Add(Transacao);
  if (FDataInicial = 0) or (Transacao.Data < FDataInicial) then
    FDataInicial := Transacao.Data;
  if (FDataFinal = 0) or (Transacao.Data > FDataFinal) then
    FDataFinal := Transacao.Data;
end;

class function TExtratoCsvReader.ValorCampo(Campos: TStrings;
  Indice: Integer): string;
begin
  if (Indice >= 0) and (Indice < Campos.Count) then
    Result := Campos[Indice]
  else
    Result := '';
end;

class function TExtratoCsvReader.EncontrarColuna(Cabecalhos: TStrings;
  const Nomes: array of string): Integer;
var
  I, J: Integer;
  Cabecalho: string;
begin
  for I := 0 to Cabecalhos.Count - 1 do
  begin
    Cabecalho := Normalizar(Cabecalhos[I]);
    for J := Low(Nomes) to High(Nomes) do
      if Cabecalho = Normalizar(Nomes[J]) then
        Exit(I);
  end;
  Result := -1;
end;

class function TExtratoCsvReader.Normalizar(const Valor: string): string;
var
  Texto: string;
  I: Integer;
  C: Char;
begin
  Texto := Valor.Trim;
  Result := '';
  for I := 1 to Texto.Length do
  begin
    case Texto[I] of
      'á', 'à', 'ã', 'â', 'ä', 'Á', 'À', 'Ã', 'Â', 'Ä': C := 'A';
      'é', 'è', 'ê', 'ë', 'É', 'È', 'Ê', 'Ë': C := 'E';
      'í', 'ì', 'î', 'ï', 'Í', 'Ì', 'Î', 'Ï': C := 'I';
      'ó', 'ò', 'õ', 'ô', 'ö', 'Ó', 'Ò', 'Õ', 'Ô', 'Ö': C := 'O';
      'ú', 'ù', 'û', 'ü', 'Ú', 'Ù', 'Û', 'Ü': C := 'U';
      'ç', 'Ç': C := 'C';
      'ñ', 'Ñ': C := 'N';
    else
      C := UpCase(Texto[I]);
    end;
    if CharInSet(C, ['A'..'Z', '0'..'9']) then
      Result := Result + C;
  end;
end;

class function TExtratoCsvReader.TryLerData(const Valor: string;
  out Data: TDate): Boolean;
var
  Texto: string;
  Dia, Mes, Ano: Word;
  DiaInt, MesInt, AnoInt: Integer;
  DataHora: TDateTime;
begin
  Result := False;
  Texto := Valor.Trim;
  if Texto.Length >= 10 then
    Texto := Texto.Substring(0, 10);

  if (Texto.Length = 10) and (Texto[3] = '/') and (Texto[6] = '/') then
  begin
    if not TryStrToInt(Copy(Texto, 1, 2), DiaInt) or
      not TryStrToInt(Copy(Texto, 4, 2), MesInt) or
      not TryStrToInt(Copy(Texto, 7, 4), AnoInt) then
      Exit;
  end
  else if (Texto.Length = 10) and (Texto[5] = '-') and (Texto[8] = '-') then
  begin
    if not TryStrToInt(Copy(Texto, 1, 4), AnoInt) or
      not TryStrToInt(Copy(Texto, 6, 2), MesInt) or
      not TryStrToInt(Copy(Texto, 9, 2), DiaInt) then
      Exit;
  end
  else
    Exit;
  if (DiaInt < Low(Word)) or (DiaInt > High(Word)) or
    (MesInt < Low(Word)) or (MesInt > High(Word)) or
    (AnoInt < Low(Word)) or (AnoInt > High(Word)) then
    Exit;
  Dia := DiaInt;
  Mes := MesInt;
  Ano := AnoInt;
  Result := TryEncodeDate(Ano, Mes, Dia, DataHora);
  if Result then
    Data := DataHora;
end;

class function TExtratoCsvReader.TryLerValor(const Texto: string;
  out Valor: Currency): Boolean;
var
  Limpo: string;
  Numero: TStringBuilder;
  I, UltimaVirgula, UltimoPonto, PosicaoDecimal: Integer;
  Negativo: Boolean;
  C: Char;
begin
  Limpo := Texto.Trim;
  Limpo := StringReplace(Limpo, #$00A0, '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, ' ', '', [rfReplaceAll]);
  Limpo := StringReplace(UpperCase(Limpo), 'BRL', '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, 'R$', '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, '$', '', [rfReplaceAll]);
  Negativo := (Limpo.Contains('-')) or
    (Limpo.StartsWith('(') and Limpo.EndsWith(')'));
  Limpo := StringReplace(Limpo, '-', '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, '+', '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, '(', '', [rfReplaceAll]);
  Limpo := StringReplace(Limpo, ')', '', [rfReplaceAll]);
  for I := 1 to Limpo.Length do
    if not CharInSet(Limpo[I], ['0'..'9', ',', '.']) then
      Exit(False);
  UltimaVirgula := Limpo.LastIndexOf(',') + 1;
  UltimoPonto := Limpo.LastIndexOf('.') + 1;
  PosicaoDecimal := 0;

  if (UltimaVirgula > 0) and (UltimoPonto > 0) then
    PosicaoDecimal := Max(UltimaVirgula, UltimoPonto)
  else if UltimaVirgula > 0 then
  begin
    if (Limpo.Length - UltimaVirgula >= 1) and
      (Limpo.Length - UltimaVirgula <= 2) then
      PosicaoDecimal := UltimaVirgula;
  end
  else if UltimoPonto > 0 then
  begin
    if (Limpo.Length - UltimoPonto >= 1) and
      (Limpo.Length - UltimoPonto <= 2) then
      PosicaoDecimal := UltimoPonto;
  end;

  Numero := TStringBuilder.Create;
  try
    for I := 1 to Limpo.Length do
    begin
      C := Limpo[I];
      if CharInSet(C, ['0'..'9']) then
        Numero.Append(C)
      else if (I = PosicaoDecimal) and CharInSet(C, [',', '.']) then
        Numero.Append('.');
    end;
    if Numero.Length = 0 then
      Exit(False);
    Result := TryStrToCurr(Numero.ToString, Valor, TFormatSettings.Invariant);
  finally
    Numero.Free;
  end;
  if Result and Negativo then
    Valor := -Abs(Valor);
end;

class function TExtratoCsvReader.EhLinhaEmBranco(Campos: TStrings): Boolean;
var
  I: Integer;
begin
  for I := 0 to Campos.Count - 1 do
    if not Campos[I].Trim.IsEmpty then
      Exit(False);
  Result := True;
end;

class function TExtratoCsvReader.EhLinhaSaldo(const Descricao: string): Boolean;
begin
  Result := Normalizar(Descricao).StartsWith('SALDO');
end;

end.
