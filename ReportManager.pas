unit ReportManager;

interface

uses
  SysUtils, Classes, DB, ADODB, xmldom, XMLIntf, msxmldom, XMLDoc, Dialogs;

type
  TReport = class(TObject)
  protected
    FName: AnsiString;
    FCaption: AnsiString;
    FSQLText: AnsiString;
  public
    constructor Create(RepName, RepCaption, RepSQLText: AnsiString);
    //--
    property Name: AnsiString read FName write FName;
    property Caption: AnsiString read FCaption write FCaption;
    property SQLText: AnsiString read FSQLText write FSQLText;
  end;

type
  TdmReportManager = class(TDataModule)
    FDataSet: TADODataSet;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FReportList: TList;
    //--
    function GetReport(const ReportName: AnsiString): TReport;
    function PrepareExcelFile(const SheetName: AnsiString): Variant;
    function GetReportList: TList;
    procedure ReadReports();
    procedure AddReport(const ReportFile: AnsiString);
  public
    { Public declarations }
    procedure DoReport(const ReportName: AnsiString);
    procedure ReportQueryResult(const DataSet: TDataSet;
      const TableName: AnsiString);
    //--
    property ReportList: TList read GetReportList;
  end;

var
  dmReportManager: TdmReportManager;

implementation

{$R *.dfm}

uses
  ComObj, Data, ForestConsts, NsUtils, Variants;

//---------------------------------------------------------------------------
{ TReport }

constructor TReport.Create(RepName, RepCaption, RepSQLText: AnsiString);
begin
  FName := RepName;
  FCaption := RepCaption;
  FSQLText := RepSQLText;
end;

//---------------------------------------------------------------------------
{ TdmReportManager }

procedure TdmReportManager.AddReport(const ReportFile: AnsiString);
var
  Rep: TReport;
  RepFile: IXMLDocument;
  RepName, RepCaption, RepSQLText: AnsiString;

begin
  RepFile := TXMLDocument.Create(nil);
  RepFile.LoadFromFile(ReportFile);

  RepName := Trim(RepFile.DocumentElement.ChildNodes.Nodes['Name'].Text);
  RepCaption := Trim(RepFile.DocumentElement.ChildNodes.Nodes['Caption'].Text);
  RepSQLText := RepFile.DocumentElement.ChildNodes.Nodes['SQLText'].Text;

  Rep := TReport.Create(RepName, RepCaption, RepSQLText);
  FReportList.Add(Rep);
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.DataModuleCreate(Sender: TObject);
begin
  FDataSet.Connection := dmData.connDB;
  FReportList := TList.Create();
  ReadReports();
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.DataModuleDestroy(Sender: TObject);
begin
  if FDataSet.Active then
    FDataSet.Close();

  FReportList.Free();
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.DoReport(const ReportName: AnsiString);
var
  Rep: TReport;

begin
  try
    try
      if FDataSet.Active then
        FDataSet.Close();

      Rep := GetReport(ReportName);

      FDataSet.CommandText := Rep.SQLText;
      FDataSet.Open();

      ReportQueryResult(FDataSet, Rep.Caption);

    except
      on E: Exception do
        ShowMsg(E_REPORT_ERROR);
    end;
  finally
    FDataSet.Close();
    dmData.connDB.Close();
  end;
end;

//---------------------------------------------------------------------------

function TdmReportManager.GetReport(const ReportName: AnsiString): TReport;
var
  I: Integer;

begin
  Result := nil;

  for I := 0 to FReportList.Count - 1 do
    if TReport(FReportList[I]).Name = ReportName then
    begin
      Result := TReport(FReportList[I]);
      Break;
    end;
end;

//---------------------------------------------------------------------------

function TdmReportManager.GetReportList: TList;
begin
  Result := FReportList;
end;

//---------------------------------------------------------------------------

function TdmReportManager.PrepareExcelFile(const SheetName: AnsiString): Variant;
var
  ExcelApp: Variant;

begin
  ExcelApp := CreateOleObject('Excel.Application');
  ExcelApp.WorkBooks.Add();
  ExcelApp.WorkBooks[1].WorkSheets[1].Name := SheetName;
  ExcelApp.Visible := True;
  Result := ExcelApp.WorkBooks[1].WorkSheets[SheetName];
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.ReadReports;
var
  SearchResult: TSearchRec;

begin
  if FindFirst(GetAppPath() + 'Reports\*.rep', faAnyFile, SearchResult) = 0 then
  begin
    repeat
      AddReport(GetAppPath() + 'Reports\' + SearchResult.Name);
    until FindNext(SearchResult) <> 0;

    FindClose(SearchResult);
  end;
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.ReportQueryResult(const DataSet: TDataSet;
  const TableName: AnsiString);
var
  Row, Col: Integer;
  Sheet: Variant;

begin
  Sheet := PrepareExcelFile(TableName);

  DataSet.First;
  for Row := 1 to DataSet.RecordCount do
  begin
    for Col := 1 to DataSet.FieldCount do
      Sheet.Cells[Row, Col] := DataSet.Fields[Col - 1].AsString;

    DataSet.Next();
  end;
end;

end.
