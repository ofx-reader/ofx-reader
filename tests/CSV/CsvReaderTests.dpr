program CsvReaderTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Math,
  System.Diagnostics,
  ofxreader in '..\..\src\ofxreader.pas',
  uExtratoCsvReader in '..\..\src\uExtratoCsvReader.pas';

procedure Verificar(Condicao: Boolean; const Mensagem: string);
begin
  if not Condicao then
    raise Exception.Create(Mensagem);
end;

procedure VerificarMoeda(const Nome: string; Atual, Esperado: Currency);
begin
  if Abs(Atual - Esperado) > 0.0001 then
    raise Exception.CreateFmt('%s: esperado %.2f, recebido %.2f',
      [Nome, Esperado, Atual]);
end;

function CaminhoFixture(const Nome: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'fixtures\' + Nome;
end;

function LerValor(const Texto: string): Currency;
begin
  if not TryStrToCurr(Texto, Result, TFormatSettings.Invariant) then
    raise Exception.Create('Valor inválido retornado pelo TOFXReader: ' + Texto);
end;

function Somar(Reader: TOFXReader): Currency;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Reader.Count - 1 do
    Result := Result + LerValor(Reader.Get(I).Value);
end;

procedure ObterPeriodo(Reader: TOFXReader; out DataInicial, DataFinal: TDateTime);
var
  I: Integer;
begin
  Verificar(Reader.Count > 0, 'O leitor não retornou movimentações.');
  DataInicial := Reader.Get(0).MovDate;
  DataFinal := DataInicial;
  for I := 1 to Reader.Count - 1 do
  begin
    if Reader.Get(I).MovDate < DataInicial then
      DataInicial := Reader.Get(I).MovDate;
    if Reader.Get(I).MovDate > DataFinal then
      DataFinal := Reader.Get(I).MovDate;
  end;
end;

procedure Importar(Reader: TOFXReader; const Arquivo: string);
begin
  Reader.OFXFile := Arquivo;
  Verificar(Reader.Import, 'O TOFXReader recusou o arquivo CSV.');
end;

procedure TestarModeloBancoBrasil;
var
  Reader: TOFXReader;
  DataInicial, DataFinal: TDateTime;
begin
  Reader := TOFXReader.Create(nil);
  try
    Importar(Reader, CaminhoFixture('banco-brasil.csv'));
    ObterPeriodo(Reader, DataInicial, DataFinal);
    Verificar(Reader.Count = 3, 'Banco do Brasil: quantidade de movimentações');
    Verificar(DataInicial = EncodeDate(2026, 7, 1),
      'Banco do Brasil: data inicial');
    Verificar(DataFinal = EncodeDate(2026, 7, 3),
      'Banco do Brasil: data final');
    VerificarMoeda('Banco do Brasil: soma', Somar(Reader), 129.55);
    Verificar(Reader.Get(0).Description =
      'Pix - Recebido | CLIENTE, TESTE',
      'Banco do Brasil: descrição e detalhes');
    Verificar(LerValor(Reader.Get(1).Value) = -100.50,
      'Banco do Brasil: tipo Saída deve definir sinal negativo');
    Verificar(Reader.Get(2).MovType = 'DEBIT',
      'Banco do Brasil: tipo de movimento normalizado');
  finally
    Reader.Free;
  end;
end;

procedure TestarUtf8PontoVirgula;
var
  Reader: TOFXReader;
begin
  Reader := TOFXReader.Create(nil);
  try
    Importar(Reader, CaminhoFixture('utf8-ponto-virgula.csv'));
    Verificar(Reader.Count = 4, 'UTF-8: quantidade de movimentações');
    VerificarMoeda('UTF-8: soma', Somar(Reader), 9.00);
    Verificar(Reader.Get(1).Description = 'Compra; material',
      'UTF-8: separador dentro de campo entre aspas');
    Verificar(Reader.Get(2).SourceKey <> Reader.Get(3).SourceKey,
      'UTF-8: transações idênticas precisam de chaves distintas');
  finally
    Reader.Free;
  end;
end;

procedure TestarCabecalhoInvalido;
var
  Reader: TOFXReader;
  Rejeitado: Boolean;
begin
  Reader := TOFXReader.Create(nil);
  try
    Rejeitado := False;
    try
      Importar(Reader, CaminhoFixture('cabecalho-invalido.csv'));
    except
      on E: Exception do
        Rejeitado := Pos('Colunas não localizadas', E.Message) > 0;
    end;
    Verificar(Rejeitado,
      'CSV incompatível deve ser rejeitado com uma mensagem objetiva');
  finally
    Reader.Free;
  end;
end;

procedure TestarFormatoLegado(const Nome: string);
var
  Reader: TOFXReader;
begin
  Reader := TOFXReader.Create(nil);
  try
    Reader.OFXFile := CaminhoFixture(Nome);
    Verificar(Reader.Import, Nome + ': formato legado não reconhecido');
    Verificar(Reader.Count > 0, Nome + ': nenhuma movimentação importada');
  finally
    Reader.Free;
  end;
end;

procedure ValidarAmostra(const Arquivo: string);
var
  Reader: TOFXReader;
  Cronometro: TStopwatch;
  DataInicial, DataFinal: TDateTime;
begin
  Reader := TOFXReader.Create(nil);
  try
    Cronometro := TStopwatch.StartNew;
    Importar(Reader, Arquivo);
    Cronometro.Stop;
    ObterPeriodo(Reader, DataInicial, DataFinal);
    Writeln(Format('AMOSTRA|%s|%d|%s|%s|%.2f|%.3fms', [
      ExtractFileName(Arquivo), Reader.Count,
      FormatDateTime('yyyy-mm-dd', DataInicial),
      FormatDateTime('yyyy-mm-dd', DataFinal), Somar(Reader),
      Cronometro.Elapsed.TotalMilliseconds]));
  finally
    Reader.Free;
  end;
end;

var
  I: Integer;
begin
  try
    TestarModeloBancoBrasil;
    TestarUtf8PontoVirgula;
    TestarCabecalhoInvalido;
    TestarFormatoLegado('regressao.ofx');
    TestarFormatoLegado('regressao.ofc');
    Writeln('OK - TOFXReader importou OFX/OFC/CSV, ignorou saldos e normalizou sinais, codificação e separadores.');
    for I := 1 to ParamCount do
      ValidarAmostra(ParamStr(I));
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('FALHA - ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
