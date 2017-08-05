object FormExample1: TFormExample1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'FormSimple'
  ClientHeight = 341
  ClientWidth = 901
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonReader: TButton
    Left = 386
    Top = 13
    Width = 137
    Height = 41
    Caption = 'Reader OFX/OFC file'
    TabOrder = 0
    OnClick = ButtonReaderClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 63
    Width = 877
    Height = 257
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Path: TLabeledEdit
    Left = 16
    Top = 32
    Width = 361
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Path'
    TabOrder = 2
    Text = '..\..\..\ofx-files\BancodoBrasil.ofx'
  end
end
