unit TestCSVReader;

interface
uses
  DUnitX.TestFramework, classes, SysUtils, ofxreader;

type
  // Test methods for CSV bank statement files
  [TestFixture]
  TestTCSVReader = class(TObject)
  strict private
    FOFXReader: TOFXReader;
    function Fixture(const Nome: string): string;
    function ValorNumerico(const Texto: string): Currency;
    function Somar: Currency;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestBancoBrasil_Quantidade;
    [Test]
    procedure TestBancoBrasil_Periodo;
    [Test]
    procedure TestBancoBrasil_Soma;
    [Test]
    procedure TestBancoBrasil_Descricao;
    [Test]
    procedure TestBancoBrasil_SinalNegativoParaSaida;
    [Test]
    procedure TestBancoBrasil_MovTypeNormalizado;
    [Test]
    procedure TestUtf8PontoVirgula_Quantidade;
    [Test]
    procedure TestUtf8PontoVirgula_Soma;
    [Test]
    procedure TestUtf8PontoVirgula_SeparadorEntreAspas;
    [Test]
    procedure TestUtf8PontoVirgula_ChavesDistintasParaDuplicatas;
    [Test]
    procedure TestCabecalhoInvalido_Rejeitado;
  end;

implementation

{ TestTCSVReader }

procedure TestTCSVReader.SetUp;
begin
  FOFXReader := TOFXReader.Create(nil);
end;

procedure TestTCSVReader.TearDown;
begin
  FOFXReader.Free;
  FOFXReader := nil;
end;

function TestTCSVReader.Fixture(const Nome: string): string;
begin
  Result := '..\fixtures\csv\' + Nome;
end;

function TestTCSVReader.ValorNumerico(const Texto: string): Currency;
begin
  if not TryStrToCurr(Texto, Result, TFormatSettings.Invariant) then
    raise Exception.Create('Valor inv�lido retornado pelo TOFXReader: ' + Texto);
end;

function TestTCSVReader.Somar: Currency;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FOFXReader.Count - 1 do
    Result := Result + ValorNumerico(FOFXReader.Get(I).Value);
end;

procedure TestTCSVReader.TestBancoBrasil_Quantidade;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(3, FOFXReader.Count);
end;

procedure TestTCSVReader.TestBancoBrasil_Periodo;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(EncodeDate(2026, 7, 1), FOFXReader.Get(0).MovDate);
  Assert.AreEqual(EncodeDate(2026, 7, 3), FOFXReader.Get(2).MovDate);
end;

procedure TestTCSVReader.TestBancoBrasil_Soma;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(129.55, Somar, 0.0001);
end;

procedure TestTCSVReader.TestBancoBrasil_Descricao;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual('Pix - Recebido | CLIENTE, TESTE', FOFXReader.Get(0).Description);
end;

procedure TestTCSVReader.TestBancoBrasil_SinalNegativoParaSaida;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(-100.50, ValorNumerico(FOFXReader.Get(1).Value), 0.0001);
end;

procedure TestTCSVReader.TestBancoBrasil_MovTypeNormalizado;
begin
  FOFXReader.OFXFile := Fixture('banco-brasil.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual('D', FOFXReader.Get(2).MovType);
end;

procedure TestTCSVReader.TestUtf8PontoVirgula_Quantidade;
begin
  FOFXReader.OFXFile := Fixture('utf8-ponto-virgula.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(4, FOFXReader.Count);
end;

procedure TestTCSVReader.TestUtf8PontoVirgula_Soma;
begin
  FOFXReader.OFXFile := Fixture('utf8-ponto-virgula.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual(9.00, Somar, 0.0001);
end;

procedure TestTCSVReader.TestUtf8PontoVirgula_SeparadorEntreAspas;
begin
  FOFXReader.OFXFile := Fixture('utf8-ponto-virgula.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreEqual('Compra; material', FOFXReader.Get(1).Description);
end;

procedure TestTCSVReader.TestUtf8PontoVirgula_ChavesDistintasParaDuplicatas;
begin
  FOFXReader.OFXFile := Fixture('utf8-ponto-virgula.csv');
  Assert.IsTrue(FOFXReader.Import);
  Assert.AreNotEqual(FOFXReader.Get(2).SourceKey, FOFXReader.Get(3).SourceKey);
end;

procedure TestTCSVReader.TestCabecalhoInvalido_Rejeitado;
var
  Rejeitado: Boolean;
begin
  FOFXReader.OFXFile := Fixture('cabecalho-invalido.csv');
  Rejeitado := False;
  try
    FOFXReader.Import;
  except
    on E: Exception do
      Rejeitado := Pos('Colunas n', E.Message) > 0;
  end;
  Assert.IsTrue(Rejeitado, 'CSV incompat�vel deve ser rejeitado com uma mensagem objetiva');
end;

initialization
  TDUnitX.RegisterTestFixture(TestTCSVReader);
end.
