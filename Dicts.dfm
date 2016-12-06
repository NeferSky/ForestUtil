object frmDicts: TfrmDicts
  Left = 668
  Top = 182
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1057#1083#1086#1074#1072#1088#1080
  ClientHeight = 417
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    409
    417)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDictionary: TLabel
    Left = 16
    Top = 8
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1057#1083#1086#1074#1072#1088#1100':'
  end
  object lblValidValues: TLabel
    Left = 16
    Top = 56
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1057#1087#1080#1089#1086#1082' '#1055#1056#1040#1042#1048#1051#1068#1053#1067#1061' '#1079#1085#1072#1095#1077#1085#1080#1081':'
  end
  object cmbDicts: TComboBox
    Left = 16
    Top = 24
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnSelect = cmbDictsSelect
  end
  object memValues: TMemo
    Left = 16
    Top = 72
    Width = 281
    Height = 289
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object btnClear: TButton
    Left = 312
    Top = 80
    Width = 81
    Height = 49
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100' '#1089#1083#1086#1074#1072#1088#1100
    TabOrder = 2
    WordWrap = True
    OnClick = btnClearClick
  end
  object btnFillFromDB: TButton
    Left = 312
    Top = 144
    Width = 81
    Height = 49
    Caption = #1047#1072#1087#1086#1083#1085#1080#1090#1100' '#1080#1079' '#1041#1044
    TabOrder = 3
    WordWrap = True
    OnClick = btnFillFromDBClick
  end
  object btnOk: TButton
    Left = 144
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnApply: TButton
    Left = 232
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
    TabOrder = 5
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 376
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 6
  end
end
