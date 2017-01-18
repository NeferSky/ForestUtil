object frmDicts: TfrmDicts
  Left = 216
  Top = 193
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = #1057#1083#1086#1074#1072#1088#1080
  ClientHeight = 433
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
    433)
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
  object lblShowFormat: TLabel
    Left = 16
    Top = 344
    Width = 281
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1082#1072#1082':'
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
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object btnClear: TButton
    Left = 312
    Top = 80
    Width = 81
    Height = 49
    Anchors = [akTop, akRight]
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
    Anchors = [akTop, akRight]
    Caption = #1047#1072#1087#1086#1083#1085#1080#1090#1100' '#1080#1079' '#1041#1044
    TabOrder = 3
    WordWrap = True
    OnClick = btnFillFromDBClick
  end
  object btnOk: TButton
    Left = 144
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = btnOkClick
  end
  object btnApply: TButton
    Left = 232
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
    TabOrder = 7
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 8
  end
  object edtShowFormat: TEdit
    Left = 16
    Top = 360
    Width = 260
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object btnFormatHelp: TButton
    Left = 276
    Top = 360
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '?'
    TabOrder = 5
    OnClick = btnFormatHelpClick
  end
end
