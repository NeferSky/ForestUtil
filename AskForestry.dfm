object frmAskForestry: TfrmAskForestry
  Left = 712
  Top = 509
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086
  ClientHeight = 169
  ClientWidth = 521
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
  object btnOk: TButton
    Left = 344
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 432
    Top = 128
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object gbxForestry: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 105
    Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086
    TabOrder = 2
    object lblForestry: TLabel
      Left = 8
      Top = 56
      Width = 321
      Height = 17
      AutoSize = False
      Caption = #1051#1077#1089#1085#1080#1095#1077#1089#1090#1074#1086':'
    end
    object lblRegion: TLabel
      Left = 8
      Top = 16
      Width = 321
      Height = 17
      AutoSize = False
      Caption = #1056#1077#1075#1080#1086#1085':'
    end
    object cmbForestry: TComboBox
      Left = 8
      Top = 72
      Width = 321
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
    object cmbRegion: TComboBox
      Left = 8
      Top = 32
      Width = 321
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = #1053#1080#1078#1077#1075#1086#1088#1086#1076#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
      OnSelect = cmbRegionSelect
      Items.Strings = (
        #1053#1080#1078#1077#1075#1086#1088#1086#1076#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
        #1050#1080#1088#1086#1074#1089#1082#1072#1103' '#1086#1073#1083#1072#1089#1090#1100)
    end
  end
  object gbxReportPeriod: TGroupBox
    Left = 352
    Top = 8
    Width = 161
    Height = 105
    Caption = #1054#1090#1095#1077#1090#1085#1099#1081' '#1087#1077#1088#1080#1086#1076
    TabOrder = 3
    object lblMonth: TLabel
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      AutoSize = False
      Caption = #1052#1077#1089#1103#1094
    end
    object lblYear: TLabel
      Left = 8
      Top = 56
      Width = 145
      Height = 17
      AutoSize = False
      Caption = #1043#1086#1076
    end
    object cmbQuarter: TComboBox
      Left = 8
      Top = 32
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'I '#1082#1074#1072#1088#1090#1072#1083
      Items.Strings = (
        'I '#1082#1074#1072#1088#1090#1072#1083
        'II '#1082#1074#1072#1088#1090#1072#1083
        'III '#1082#1074#1072#1088#1090#1072#1083
        'IV '#1082#1074#1072#1088#1090#1072#1083)
    end
    object edtYear: TEdit
      Left = 8
      Top = 72
      Width = 127
      Height = 21
      TabOrder = 1
      Text = '1999'
    end
    object udYear: TUpDown
      Left = 135
      Top = 72
      Width = 17
      Height = 21
      Associate = edtYear
      Min = 1999
      Max = 2099
      Position = 1999
      TabOrder = 2
      Thousands = False
    end
  end
end
