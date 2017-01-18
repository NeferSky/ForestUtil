object frmEdit: TfrmEdit
  Left = 478
  Top = 190
  Width = 393
  Height = 243
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
    385
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTableValue: TLabel
    Left = 8
    Top = 56
    Width = 361
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1080#1079' '#1090#1072#1073#1083#1080#1094#1099':'
    WordWrap = True
  end
  object lblSynonim: TLabel
    Left = 8
    Top = 104
    Width = 361
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1085#1072' '#1079#1085#1072#1095#1077#1085#1080#1077':'
  end
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 361
    Height = 41
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Prompt'
  end
  object edtWord: TEdit
    Left = 8
    Top = 72
    Width = 361
    Height = 24
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    Text = 'Text'
  end
  object btnOk: TButton
    Left = 24
    Top = 160
    Width = 75
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object cmbSynonim: TComboBox
    Left = 8
    Top = 120
    Width = 361
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmbSynonimChange
    OnDropDown = cmbSynonimDropDown
    OnKeyPress = cmbSynonimKeyPress
    OnSelect = cmbSynonimChange
  end
  object btnSkip: TButton
    Left = 112
    Top = 160
    Width = 75
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100
    ModalResult = 7
    TabOrder = 3
    OnClick = btnSkipClick
  end
  object btnSkipAll: TButton
    Left = 200
    Top = 160
    Width = 75
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100' '#1074#1089#1077
    ModalResult = 9
    TabOrder = 4
    WordWrap = True
    OnClick = btnSkipAllClick
  end
  object btnStop: TButton
    Left = 288
    Top = 160
    Width = 75
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100
    ModalResult = 3
    TabOrder = 5
    OnClick = btnStopClick
  end
end
