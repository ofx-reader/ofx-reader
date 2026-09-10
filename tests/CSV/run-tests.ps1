param(
    [string]$OutputRoot = 'G:\Desenvolvimento\OFX-Reader_csv_20260910\regressao',
    [string[]]$SampleFiles = @()
)

$ErrorActionPreference = 'Stop'
$testRoot = $PSScriptRoot
$resolvedOutput = [IO.Path]::GetFullPath($OutputRoot)
if (-not $resolvedOutput.StartsWith('G:\Desenvolvimento\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'A saída dos testes deve ficar em G:\Desenvolvimento.'
}

New-Item -ItemType Directory -Path $resolvedOutput -Force | Out-Null
$fixtureOutput = Join-Path $resolvedOutput 'fixtures'
New-Item -ItemType Directory -Path $fixtureOutput -Force | Out-Null
Copy-Item (Join-Path $testRoot 'fixtures\*') $fixtureOutput -Force
Copy-Item (Join-Path $testRoot '..\..\ofx-files\BancodoBrasil.ofx') (Join-Path $fixtureOutput 'regressao.ofx') -Force
Copy-Item (Join-Path $testRoot '..\..\ofx-files\extrato.ofc') (Join-Path $fixtureOutput 'regressao.ofc') -Force

$namespaces = 'Vcl;Vcl.Imaging;System;System.Win;Xml;Xml.Win;Data;Data.Win;Datasnap;Datasnap.Win;Web;Web.Win;Soap;Soap.Win;Winapi'
$command = 'call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" && ' +
    'cd /d "' + $testRoot + '" && ' +
    '"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\dcc32.exe" -B -Q ' +
    '-E"' + $resolvedOutput + '" -N0"' + $resolvedOutput + '" -NS"' + $namespaces +
    '" "CsvReaderTests.dpr"'
& cmd.exe /c $command
if ($LASTEXITCODE -ne 0) { throw "Falha na compilação: $LASTEXITCODE" }

& (Join-Path $resolvedOutput 'CsvReaderTests.exe') @SampleFiles
if ($LASTEXITCODE -ne 0) { throw "Falha nos testes: $LASTEXITCODE" }
