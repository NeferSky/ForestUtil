object dmReportManager: TdmReportManager
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 731
  Top = 127
  Height = 269
  Width = 215
  object FDataSet: TADODataSet
    Connection = dmData.connDB
    CursorType = ctStatic
    CommandText = 'select * from species'
    Parameters = <>
    Left = 64
    Top = 48
  end
end
