object frmAskForestry: TfrmAskForestry
  Left = 673
  Top = 237
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086
  ClientHeight = 146
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    353
    146)
  PixelsPerInch = 96
  TextHeight = 13
  object lblForestry: TLabel
    Left = 16
    Top = 56
    Width = 321
    Height = 17
    AutoSize = False
    Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086':'
  end
  object lblRegion: TLabel
    Left = 16
    Top = 8
    Width = 321
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1056#1077#1075#1080#1086#1085':'
  end
  object cmbForestry: TComboBox
    Left = 16
    Top = 72
    Width = 321
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 168
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 256
    Top = 104
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
  end
  object cmbRegion: TComboBox
    Left = 16
    Top = 24
    Width = 321
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = #1053#1080#1078#1077#1075#1086#1088#1086#1076#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
    OnSelect = cmbRegionSelect
    Items.Strings = (
      #1053#1080#1078#1077#1075#1086#1088#1086#1076#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
      #1050#1080#1088#1086#1074#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100)
  end
end
