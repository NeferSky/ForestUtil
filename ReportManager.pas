unit ReportManager;

interface

uses
  Classes, Variants, ADODB, DB, SysUtils, xmldom, XMLIntf, msxmldom, XMLDoc;

type
  TReport = class(TObject)
  protected
    FName: AnsiString;
    FCaption: AnsiString;
    FSQLText: AnsiString;
  public
    constructor Create(AOwner: TComponent);
    //--
    property Name: AnsiString read FName write FName;
    property Caption: AnsiString read FCaption write FCaption;
    property SQLText: AnsiString read FSQLText write FSQLText;
  end;

type
  TReportManager = class(TObject)
  private
    FDataSet: TADODataSet;
    FReportList: TList;
    //--
    function GetReport(const ReportName: AnsiString): TReport;
    function PrepareExcelFile(const SheetName: AnsiString): Variant;
    function GetReportList: TStringList;
    procedure ReadReports();
    procedure AddReport(const ReportFile: AnsiString);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure DoReport(const ReportName: AnsiString);
    procedure ReportQueryResult(const DataSet: TDataSet;
      const TableName: AnsiString);
    property ReportList: TStringList read GetReportList;
  end;

implementation

uses
  ComObj, Data, ForestConsts, NsUtils;

//---------------------------------------------------------------------------
{ TReportManager }

procedure TReportManager.AddReport(const ReportFile: AnsiString);
var
  Rep: TReport;
  RepFile: IXMLDocument;

begin
  Rep := TReport.Create(nil);
  RepFile.LoadFromFile(ReportFile);

  Rep.Name := RepFile.ChildNodes['NAME'].Text;
  Rep.Caption := RepFile.ChildNodes['CAPTION'].Text;
  Rep.SQLText := RepFile.ChildNodes['SQLTEXT'].Text;

  FReportList.Add(Rep);
end;

//---------------------------------------------------------------------------

constructor TReportManager.Create(AOwner: TComponent);
begin
{  FDataSet := TADODataSet.Create(AOwner);
  FDataSet.Connection := FConnection;
  FDataSet.CommandText := 'select * from quarterreports';

  FDataSet.Open();
  FReportList := TList.Create();

  ReadReports();
  }
end;

//---------------------------------------------------------------------------

destructor TReportManager.Destroy;
begin
{  if FDataSet.Active then
    FDataSet.Close();
//  FDataSet.Free();

//  FConnection.Close();
//  FConnection.Free();

  FReportList.Clear();
  FReportList.Free();
  }
end;

//---------------------------------------------------------------------------

procedure TReportManager.DoReport(const ReportName: AnsiString);
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
  end;
end;

//---------------------------------------------------------------------------

function TReportManager.GetReport(const ReportName: AnsiString): TReport;
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

function TReportManager.GetReportList: TStringList;
var
  I: Integer;

begin
  Result.Clear();

  for I := 0 to FReportList.Count - 1 do
    Result.Add(TReport(FReportList[I]).Caption);
end;

//---------------------------------------------------------------------------

function TReportManager.PrepareExcelFile(const SheetName: AnsiString): Variant;
var
  ExcelApp: Variant;

begin
  ExcelApp := CreateOleObject('Excel.Application');
  ExcelApp.Visible := True;
  ExcelApp.WorkBooks.Add();
  ExcelApp.WorkBooks[1].WorkSheets[1].Name := SheetName;
  Result := ExcelApp.WorkBooks[1].WorkSheets[SheetName];
end;

//---------------------------------------------------------------------------

procedure TReportManager.ReadReports;
var
  SearchResult: TSearchRec;

begin
  if FindFirst('*.rep', faAnyFile, SearchResult) = 0 then
  begin
    repeat
      AddReport(GetAppPath() + 'Reports\' + SearchResult.Name);
    until FindNext(SearchResult) <> 0;

    FindClose(SearchResult);
  end;
end;

//---------------------------------------------------------------------------

procedure TReportManager.ReportQueryResult(const DataSet: TDataSet;
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

//---------------------------------------------------------------------------
{ TReport }

constructor TReport.Create(AOwner: TComponent);
begin
  FName := '';
  FCaption := '';
  FSQLText := '';
end;

end.
