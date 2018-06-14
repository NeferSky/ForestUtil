object dmData: TdmData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 329
  Top = 194
  Height = 435
  Width = 245
  object connDB: TADOConnection
    ConnectionString = 
      'Provider=MSDASQL.1;Password=9049225894;Persist Security Info=Tru' +
      'e;User ID=postgres;Extended Properties="DRIVER={PostgreSQL ANSI}' +
      ';DATABASE=maximfo;SERVER=localhost;PORT=5432;UID=postgres;PWD=90' +
      '49225894;SSLmode=disable;ReadOnly=0;Protocol=7.4;FakeOidIndex=0;' +
      'ShowOidColumn=0;RowVersioning=0;ShowSystemTables=0;ConnSettings=' +
      ';Fetch=100;Socket=4096;UnknownSizes=0;MaxVarcharSize=255;MaxLong' +
      'VarcharSize=8190;Debug=0;CommLog=0;Optimizer=0;Ksqo=1;UseDeclare' +
      'Fetch=0;TextAsLongVarchar=1;UnknownsAsLongVarchar=0;BoolsAsChar=' +
      '1;Parse=0;CancelAsFreeStmt=0;ExtraSysTablePrefixes=dd_;;LFConver' +
      'sion=1;UpdatableCursors=1;DisallowPremature=0;TrueIsMinus1=0;BI=' +
      '0;ByteaAsLongVarBinary=0;UseServerSidePrepare=1;LowerCaseIdentif' +
      'ier=0;GssAuthUseGSS=0;XaOpt=1";Initial Catalog=maximfo'
    LoginPrompt = False
    Provider = 'MSDASQL.1'
    Left = 40
    Top = 24
  end
  object qryFileSelect: TADOQuery
    Connection = connFile
    Parameters = <>
    SQL.Strings = (
      'select * from ['#1051#1080#1089#1090'1$]')
    Left = 152
    Top = 72
  end
  object qryCommand: TADOQuery
    Connection = connDB
    Parameters = <>
    Left = 40
    Top = 72
  end
  object connFile: TADOConnection
    LoginPrompt = False
    Left = 152
    Top = 24
  end
  object qryGetTableValues: TADOQuery
    Connection = connDB
    Parameters = <>
    Left = 40
    Top = 216
  end
  object mdCache: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 152
    Top = 216
  end
  object qrySelect: TADOQuery
    Connection = connDB
    Parameters = <>
    Left = 40
    Top = 120
  end
end
