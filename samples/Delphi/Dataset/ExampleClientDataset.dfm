object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object Path: TLabeledEdit
    Left = 8
    Top = 24
    Width = 361
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Path'
    TabOrder = 0
    Text = '..\..\..\ofx-files\BancodoBrasil.ofx'
  end
  object Button1: TButton
    Left = 375
    Top = 22
    Width = 114
    Height = 25
    Caption = 'OFX/OFC 2 Dataset'
    TabOrder = 1
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 51
    Width = 619
    Height = 240
    Anchors = [akLeft, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 304
    Top = 96
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 232
    Top = 96
  end
end
