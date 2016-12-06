object frmEdit: TfrmEdit
  Left = 470
  Top = 163
  Width = 393
  Height = 187
  BorderIcons = []
  Caption = #1042#1074#1086#1076' '#1079#1072#1084#1077#1085#1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    377
    148)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTableValue: TLabel
    Left = 8
    Top = 8
    Width = 369
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1080#1079' '#1090#1072#1073#1083#1080#1094#1099':'
    WordWrap = True
  end
  object lblSynonim: TLabel
    Left = 8
    Top = 56
    Width = 369
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1085#1072' '#1079#1085#1072#1095#1077#1085#1080#1077':'
  end
  object edtWord: TEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    Text = 'Text'
  end
  object btnOk: TButton
    Left = 208
    Top = 112
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object cmbSynonim: TComboBox
    Left = 8
    Top = 72
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnSkip: TButton
    Left = 296
    Top = 112
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100
    ModalResult = 7
    TabOrder = 3
  end
end
