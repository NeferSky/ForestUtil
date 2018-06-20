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
    FHeaderSQLText: AnsiString;
    FColumns: TStringList;
    FFooterSQLText: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    //--
    property Name: AnsiString read FName write FName;
    property Caption: AnsiString read FCaption write FCaption;
    property SQLText: AnsiString read FSQLText write FSQLText;
    property HeaderSQLText: AnsiString read FHeaderSQLText write FHeaderSQLText;
    property Columns: TStringList read FColumns write FColumns;
    property FooterSQLText: AnsiString read FFooterSQLText write FFooterSQLText;
  end;

type
  TdmReportManager = class(TDataModule)
    FDataSet: TADODataSet;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FRow: Integer;
    FReportList: TList;
    //--
    function GetReport(const ReportName: AnsiString): TReport;
    function PrepareExcelFile(const SheetName: AnsiString): Variant;
    function GetReportList: TList;
    procedure ReadReports();
    procedure AddReport(const ReportFile: AnsiString);
    procedure ReportQueryResult(const Sheet: Variant; const SQLText: AnsiString);
    procedure ReportString(const Sheet: Variant; const Text: AnsiString);
    procedure ReportStringListH(const Sheet: Variant; const TextList: TStringList);
    procedure ReportStringListV(const Sheet: Variant; const TextList: TStringList);
  public
    { Public declarations }
    procedure DoReport(const ReportName: AnsiString);
    procedure ReportUIQuery(const DataSet: TDataSet; const TableName: AnsiString);
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

constructor TReport.Create;
begin
  FName := '';
  FCaption := '';
  FSQLText := '';
  FColumns := TStringList.Create();
  FHeaderSQLText := '';
  FFooterSQLText := '';
end;

//---------------------------------------------------------------------------

destructor TReport.Destroy;
begin
  FColumns.Free();

  inherited;
end;

//---------------------------------------------------------------------------
{ TdmReportManager }

procedure TdmReportManager.AddReport(const ReportFile: AnsiString);
var
  Rep: TReport;
  RepFile: IXMLDocument;
  RepColumns: TStringList;

  function ReadStr(Section: AnsiString): AnsiString;
  begin
    try
      Result := Trim(RepFile.DocumentElement.ChildNodes.Nodes[Section].Text);
    except
      Result := '';
    end;
  end;

  function ReadInt(Section: AnsiString): Integer;
  begin
    try
      Result := StrToInt(Trim(RepFile.DocumentElement.ChildNodes.Nodes[Section].Text));
    except
      Result := 0;
    end;
  end;

  procedure ReadStrings(Section: AnsiString; var AStringList: TStringList);
  begin
    try
      AStringList.DelimitedText := Trim(RepFile.DocumentElement.ChildNodes.Nodes[Section].Text);
    except
      AStringList.Clear();
    end;
  end;

begin
  Rep := TReport.Create();
  RepColumns := TStringList.Create();
  RepColumns.Delimiter := '#';
  RepColumns.QuoteChar := '|';

  RepFile := TXMLDocument.Create(nil);
  RepFile.LoadFromFile(ReportFile);

  Rep.Name := ReadStr('Name');
  Rep.Caption := ReadStr('Caption');
  Rep.SQLText := ReadStr('SQLText');
  Rep.HeaderSQLText := ReadStr('HeaderSQLText');
  ReadStrings('Columns', RepColumns);
  Rep.Columns.AddStrings(RepColumns);
  Rep.FooterSQLText := ReadStr('FooterSQLText');

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
  Sheet: Variant;

begin
  try
    try
      Rep := GetReport(ReportName);
      Sheet := PrepareExcelFile(Rep.Caption);
      FRow := 1;

      ReportString(Sheet, Rep.Caption);
      ReportQueryResult(Sheet, Rep.HeaderSQLText);
      ReportStringListH(Sheet, Rep.Columns);
      ReportQueryResult(Sheet, Rep.SQLText);
      ReportQueryResult(Sheet, Rep.FFooterSQLText);

    except
      on E: Exception do
        ShowMsg(E_REPORT_ERROR);
    end;
  finally
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

procedure TdmReportManager.ReportQueryResult(const Sheet: Variant; const SQLText: AnsiString);
var
  CurRow, CurCol: Integer;

begin
  try
    if SQLText = '' then
      Exit;

    if FDataSet.Active then
      FDataSet.Close();

    FDataSet.CommandText := SQLText;
    FDataSet.Open();
    FDataSet.First();
    CurRow := 1;

    while CurRow <= FDataSet.RecordCount do
    begin
      CurCol := 1;
      while CurCol <= FDataSet.FieldCount do
      begin
        Sheet.Cells[FRow, CurCol] := FDataSet.Fields[CurCol - 1].AsString;
        Inc(CurCol);
      end;

      FDataSet.Next();
      Inc(CurRow);
      Inc(FRow);
    end;

  finally
    FDataSet.Close();
  end;
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.ReportUIQuery(const DataSet: TDataSet; const TableName: AnsiString);
var
  CurRow, CurCol: Integer;
  Sheet: Variant;

begin
  try
    Sheet := PrepareExcelFile(TableName);

    DataSet.First();
    CurRow := 1;

    while CurRow <= DataSet.RecordCount do
    begin
      CurCol := 1;
      while CurCol <= DataSet.FieldCount do
      begin
        Sheet.Cells[FRow, CurCol] := DataSet.Fields[CurCol - 1].AsString;
        Inc(CurCol);
      end;

      DataSet.Next();
      Inc(CurRow);
    end;

  except
    on E: Exception do
      ShowMsg(E_REPORT_ERROR);
  end;
end;
  
//---------------------------------------------------------------------------

procedure TdmReportManager.ReportString(const Sheet: Variant; const Text: AnsiString);
begin
  Sheet.Cells[FRow, 1] := Text;
  Inc(FRow);
end;

//---------------------------------------------------------------------------

procedure TdmReportManager.ReportStringListH(const Sheet: Variant; const TextList: TStringList);
var
  CurCol: Integer;

begin
  CurCol := 1;

  while CurCol <= TextList.Count do
  begin
    Sheet.Cells[FRow, CurCol] := TextList[CurCol - 1];
    Inc(CurCol);
  end;

  Inc(FRow);
end;
  
//---------------------------------------------------------------------------

procedure TdmReportManager.ReportStringListV(const Sheet: Variant; const TextList: TStringList);
var
  CurRow: Integer;

begin
  CurRow := 1;
  while CurRow <= TextList.Count do
  begin
    Sheet.Cells[FRow, 1] := TextList[CurRow - 1];
    Inc(FRow);
    Inc(CurRow);
  end;
end;

end.
