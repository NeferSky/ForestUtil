object dmData: TdmData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 944
  Top = 179
  Height = 435
  Width = 245
  object connDB: TADOConnection
    ConnectionString = 
      'Provider=MSDASQL.1;Password=123;Persist Security Info=True;User ' +
      'ID=postgres;Extended Properties="DRIVER={PostgreSQL ANSI};DATABA' +
      'SE=maximfo;SERVER=localhost;PORT=5432;UID=postgres;PWD=904922589' +
      '4;SSLmode=disable;ReadOnly=0;Protocol=7.4;FakeOidIndex=0;ShowOid' +
      'Column=0;RowVersioning=0;ShowSystemTables=0;ConnSettings=;Fetch=' +
      '100;Socket=4096;UnknownSizes=0;MaxVarcharSize=255;MaxLongVarchar' +
      'Size=8190;Debug=0;CommLog=0;Optimizer=0;Ksqo=1;UseDeclareFetch=0' +
      ';TextAsLongVarchar=1;UnknownsAsLongVarchar=0;BoolsAsChar=1;Parse' +
      '=0;CancelAsFreeStmt=0;ExtraSysTablePrefixes=dd_;;LFConversion=1;' +
      'UpdatableCursors=1;DisallowPremature=0;TrueIsMinus1=0;BI=0;Bytea' +
      'AsLongVarBinary=0;UseServerSidePrepare=1;LowerCaseIdentifier=0;G' +
      'ssAuthUseGSS=0;XaOpt=1";Initial Catalog=maximfo'
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
  object mtCache: TMemTableEh
    FetchAllOnOpen = True
    Params = <>
    DataDriver = qrySelect
    Left = 40
    Top = 168
  end
  object qrySelect: TADODataDriverEh
    ADOConnection = connDB
    SelectCommand.Parameters = <>
    UpdateCommand.Parameters = <>
    InsertCommand.Parameters = <>
    DeleteCommand.Parameters = <>
    GetrecCommand.Parameters = <>
    Left = 40
    Top = 120
  end
  object qryGetTableValues: TADOQuery
    Connection = connDB
    Parameters = <>
    Left = 40
    Top = 216
  end
end
