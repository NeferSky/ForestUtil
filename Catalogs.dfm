object frmCatalogs: TfrmCatalogs
  Left = 645
  Top = 189
  Width = 417
  Height = 467
  BorderIcons = []
  Caption = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080' '#1072#1074#1090#1086#1079#1072#1084#1077#1085#1099
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
  object lblCatalog: TLabel
    Left = 16
    Top = 8
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082':'
  end
  object lblValues: TLabel
    Left = 16
    Top = 56
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1057#1087#1080#1089#1086#1082' '#1079#1085#1072#1095#1077#1085#1080#1081':'
  end
  object cmbCatalogs: TComboBox
    Left = 16
    Top = 24
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnSelect = cmbCatalogsSelect
  end
  object btnOk: TButton
    Left = 144
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnApply: TButton
    Left = 232
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 3
  end
  object lvValues: TListView
    Left = 16
    Top = 72
    Width = 281
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = #1063#1090#1086' '#1079#1072#1084#1077#1085#1103#1090#1100
      end
      item
        AutoSize = True
        Caption = #1053#1072' '#1095#1090#1086' '#1079#1072#1084#1077#1085#1103#1090#1100
      end
      item
        Caption = #1055#1088#1080' '#1091#1089#1083#1086#1074#1080#1080
        Width = 0
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowWorkAreas = True
    TabOrder = 4
    ViewStyle = vsReport
    OnClick = lvValuesClick
    OnColumnClick = lvValuesColumnClick
    OnCompare = lvValuesCompare
    OnDblClick = lvValuesDblClick
  end
  object btnClear: TButton
    Left = 312
    Top = 80
    Width = 81
    Height = 49
    Anchors = [akTop, akRight]
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082
    TabOrder = 5
    WordWrap = True
    OnClick = btnClearClick
  end
  object btnEdit: TButton
    Left = 312
    Top = 144
    Width = 81
    Height = 49
    Anchors = [akTop, akRight]
    Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077
    TabOrder = 6
    WordWrap = True
    OnClick = btnEditClick
  end
  object btnAdd: TButton
    Left = 312
    Top = 208
    Width = 81
    Height = 49
    Anchors = [akTop, akRight]
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 7
    WordWrap = True
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 312
    Top = 272
    Width = 81
    Height = 49
    Anchors = [akTop, akRight]
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077
    TabOrder = 8
    WordWrap = True
    OnClick = btnDeleteClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 414
    Width = 409
    Height = 19
    Panels = <>
  end
end
