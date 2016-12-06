object frmAskForestry: TfrmAskForestry
  Left = 568
  Top = 356
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086
  ClientHeight = 167
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblForestry: TLabel
    Left = 16
    Top = 16
    Width = 321
    Height = 17
    AutoSize = False
    Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086':'
  end
  object lblLocalForestry: TLabel
    Left = 16
    Top = 64
    Width = 321
    Height = 17
    AutoSize = False
    Caption = #1059#1095#1072#1089#1090#1082#1086#1074#1086#1077' '#1083#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086':'
  end
  object cmbForestry: TComboBox
    Left = 16
    Top = 32
    Width = 321
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnSelect = cmbForestrySelect
  end
  object cmbLocalForestry: TComboBox
    Left = 16
    Top = 80
    Width = 321
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnSelect = cmbLocalForestrySelect
  end
  object btnOk: TButton
    Left = 256
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
