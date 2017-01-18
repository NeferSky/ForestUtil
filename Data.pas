unit Data;

{//$DEFINE DEBUG}

interface

uses
  Windows, SysUtils, Classes, DB, ADODB, MemTableDataEh, DataDriverEh,
  ADODataDriverEh, MemTableEh, Validator, DBScript, ForestTypes;

type
  TdmData = class(TDataModule)
    connDB: TADOConnection;
    qryCommand: TADOQuery;
    connFile: TADOConnection;
    qryFileSelect: TADOQuery;
    mtCache: TMemTableEh;
    qrySelect: TADODataDriverEh;
    qryGetTableValues: TADOQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FInProgress: Boolean;
    FFirstRow: Integer;
    FLastRow: Integer;
    FFirstCol: Integer;
    FTableList: TStringList;
    FScript: TDBScript;
    FOnValidationLog: TOnValidationLog;
    FOnProgress: TOnProgress;
    FCurrentRecord: TValuesRec;
    FValidationResult: TValidationResult;
    FValidator: TValidator;
    FLogDetails: TLogDetails;
    FSkippedRecs: TSkippedRecs;
    FInvalidRecs: TSkippedRecs;
    //--
    procedure ReadSettings;
    procedure WriteSettings;
    //
    procedure Connect;
    procedure Disconnect;
    procedure Commit;
    procedure Rollback;
    //
    function SearchForDuplicates: TValidationRes;
    procedure ReadDBString(const RegionID, ForestryID, ReportQuarter,
      ReportYear: Integer);
    //
    procedure ValidationLog(Str: AnsiString);
    procedure Progress(CurrentIteration: Integer);
    function FindFirstRow: Boolean;
    function FindLastRow: Boolean;
    function EmptyRec: Boolean;
    function MoveTo(const DataSet: TDataSet; const Position: Integer): Boolean;
    //
    function ExistsSameReport(const ForestryID, ReportQuarter, ReportYear:
      Integer): Boolean;
    function ExistsPrevReport(const ForestryID: Integer; ReportQuarter,
      ReportYear: Integer): Boolean;
    function GetPrevReportSums(const ForestryID, ReportYear, ReportQuarter:
      Integer; const PrevYear: Boolean): TReportSums;
    //
    procedure AddSkippedRec(RecNo: Integer);
    procedure AddInvalidRec(RecNo: Integer);
    procedure RemoveSkippedRec(RecNo: Integer);
    //
    function GetDBConnectionString: AnsiString;
    function GetFileConnectionString(const FileName: AnsiString): AnsiString;
    function GetTableList: TStringList;
    function GetInProgress: Boolean;
    function GetLogDetails: TLogDetails;
    procedure SetLogDetails(const Value: TLogDetails);
    function GetScript: AnsiString;
  public
    procedure Log(S: string);
    { Public declarations }
    function GetValuesFromTable(const DictCaption: AnsiString): TDictArr;
    function GetForestryByRegion(const RegionID: Integer): TStringList;
    function GetIntField(SQLText: AnsiString): Integer;
    procedure MoveNextSkipped;
    procedure MovePrevSkipped;
    function SkippedRow(RecNo: Integer): Boolean;
    function InvalidRow(RecNo: Integer): Boolean;
    //
    procedure ExecuteQuery(const SQLText: AnsiString);
    procedure GetQueryResult(const SQLText: AnsiString);
    function GetQueryRecordsCount: Integer;
    //
    function OpenFile(const FileName: AnsiString): Boolean;
    function OpenTable(const TableName: AnsiString): Boolean;
    function GetFileRecordsCount: Integer;
    //
    procedure ValidateTable(const RegionID, ForestryID, ReportYear,
      ReportQuarter: Integer);
    procedure ValidateRecord(const RegionID, ForestryID, ReportYear,
      ReportQuarter: Integer);
    procedure PositionTable;
    //
    property TableList: TStringList read GetTableList;
    property InProgress: Boolean read GetInProgress;
    property OnValidationLog: TOnValidationLog read FOnValidationLog write
      FOnValidationLog;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property ValidationResult: TValidationResult read FValidationResult;
    property LogDetails: TLogDetails read GetLogDetails write SetLogDetails;
    property SkippedRecs: TSkippedRecs read FSkippedRecs;
    property FirstRecNo: Integer read FFIrstRow;
    property LastRecNo: Integer read FLastRow;
    property Script: AnsiString read GetScript;
    property Validator: TValidator read FValidator;
  end;

var
  dmData: TdmData;

implementation

uses
  ForestConsts, IniFiles, Forms, NsUtils;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TdmData }

procedure TdmData.AddInvalidRec(RecNo: Integer);
var
  InvalidRecsCount: Integer;

begin
  InvalidRecsCount := Length(FInvalidRecs);
  SetLength(FInvalidRecs, InvalidRecsCount + 1);
  FInvalidRecs[InvalidRecsCount] := RecNo;
end;

//---------------------------------------------------------------------------

procedure TdmData.AddSkippedRec(RecNo: Integer);
var
  SkippedRecsCount: Integer;

begin
  SkippedRecsCount := Length(FSkippedRecs);
  SetLength(FSkippedRecs, SkippedRecsCount + 1);
  FSkippedRecs[SkippedRecsCount] := RecNo;
end;

//---------------------------------------------------------------------------

procedure TdmData.Commit;
begin
  connDB.CommitTrans();
end;

//---------------------------------------------------------------------------

procedure TdmData.Connect;
begin
  Disconnect();
  connDB.ConnectionString := GetDBConnectionString();
  connDB.Open();
  connDB.BeginTrans();
end;

//---------------------------------------------------------------------------

procedure TdmData.DataModuleCreate(Sender: TObject);
begin
  FInProgress := False;
  FTableList := TStringList.Create();
  FScript := TDBScript.Create();
  ReadSettings();
  FValidator := TValidator.Create(Application);
end;

//---------------------------------------------------------------------------

procedure TdmData.DataModuleDestroy(Sender: TObject);
begin
  WriteSettings();
  FTableList.Free();
  FScript.Free();
  FValidator.Free();
end;

//---------------------------------------------------------------------------

procedure TdmData.Disconnect;
begin
  if connDB.Connected then
    connDB.Close();
end;

//---------------------------------------------------------------------------

function TdmData.EmptyRec: Boolean;
var
  Weight: Integer;
  I: Integer;

begin
  Weight := 0;

  for I := 0 to I_COLS_TO_FIND_LAST_ROW - 1 do
  begin
    // If cell is empty...
    if Trim(qryFileSelect.Fields[I + FFirstCol].AsString) = '' then
      Inc(Weight)
        // ...else if digit cells contains non-digit values
    else if ((I = 2) or (I = 3) or (I = 9) or (I = 10)) then
      try
        StrToInt(Trim(qryFileSelect.Fields[I + FFirstCol].AsString));
      except
        Inc(Weight);
      end
  end;

  Result := Weight > I_WEIGHT_TO_FIND_LAST_ROW;
end;

//---------------------------------------------------------------------------

procedure TdmData.ExecuteQuery(const SQLText: AnsiString);
begin
  if qryCommand.Active then
    qryCommand.Close();

  try
    try
      Connect();
      qryCommand.SQL.Text := SQLText;
      qryCommand.ExecSQL();
      Commit();
      ShowMsg(S_QUERY_EXEC_SUCCESS);
    except
      Rollback();
      ShowMsg(E_QUERY_EXEC_ERROR);
    end;

  finally
    qryCommand.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.ExistsPrevReport(const ForestryID: Integer; ReportQuarter,
  ReportYear: Integer): Boolean;
begin
  case ReportQuarter of
    1:
      begin
        ReportQuarter := 4;
        ReportYear := ReportYear - 1;
      end;

  else
    begin
      ReportQuarter := ReportQuarter - 1;
    end;
  end;

  Result := GetIntField(Format(S_DB_GET_REPORT_ROW_COUNT, [S_DB_TABLE_NAME,
    ForestryID, ReportQuarter, ReportYear])) > 0;
end;

//---------------------------------------------------------------------------

function TdmData.ExistsSameReport(const ForestryID, ReportQuarter, ReportYear:
  Integer): Boolean;
begin
  Result := GetIntField(Format(S_DB_GET_REPORT_ROW_COUNT, [S_DB_TABLE_NAME,
    ForestryID, ReportQuarter, ReportYear])) > 0;
end;

//---------------------------------------------------------------------------

function TdmData.FindFirstRow: Boolean;
var
  Weight: Integer;
  I: Integer;
  ColShift: Integer;

begin
  Result := False;
  qryFileSelect.DisableControls();

  // loop for horizontal shift for cells checking
  for ColShift := 0 to (qryFileSelect.FieldCount - I_COL_COUNT) do
  begin

    qryFileSelect.First();
    // main "vertical" loop
    while not qryFileSelect.Eof do
    begin
      Progress(qryFileSelect.RecNo);
      Weight := 0;

      // cells checking
      for I := 0 to I_COL_COUNT - 1 do
        if qryFileSelect.Fields[I + ColShift].AsString = IntToStr(I + 1) then
          Inc(Weight);

      // if check sucessfull - break main "vertical" loop
      if Weight > (I_COL_COUNT div 2) then
      begin
        FFirstRow := qryFileSelect.RecNo + 1;
        FFirstCol := ColShift;
        Result := True;
        Break;
      end;

      qryFileSelect.Next();
    end;

    // if check sucessfull - break "horizontal" loop
    if Result then
      Break;
  end;

  qryFileSelect.First();
  qryFileSelect.EnableControls();
end;

//---------------------------------------------------------------------------

function TdmData.FindLastRow: Boolean;
begin
  Result := False;
  qryFileSelect.DisableControls();

  qryFileSelect.Last();

  while not qryFileSelect.Bof do
  begin

    if (not EmptyRec()) or (qryFileSelect.RecNo = FFirstRow - 1) then
    begin
      FLastRow := qryFileSelect.RecNo;
      Result := True;
      Break;
    end;

    qryFileSelect.Prior();
  end;

  qryFileSelect.First();
  qryFileSelect.EnableControls();
end;

//---------------------------------------------------------------------------

function TdmData.GetDBConnectionString: AnsiString;
begin
  Result := Format(S_POSTGRESQL_FORMAT, [S_PG_PROVIDER, S_PG_PASSWORD,
    PgPassword, S_PG_SECURITY_INFO, S_PG_USER_ID, PgUID, S_PG_EXTENDED_PROPS,
      S_PG_DRIVER, S_PG_DATABASE, PgDatabase, S_PG_SERVER, PgServer,
      S_PG_PORT, PgPort, S_PG_UID, PgUID, S_PG_PWD, PgPassword, S_PG_SSL_MODE,
      S_PG_READ_ONLY, S_PG_PROTOCOL, S_PG_FAKE_OID_INDEX, S_PG_SHOW_OID_COLS,
      S_PG_ROW_VERSIONONG, S_PG_SHOW_SYS_TABLES, S_PG_CONN_SETTINGS, S_PG_FETCH,
      S_PG_SOCKET, S_PG_UNKNOWN_SIZES, S_PG_VARCHAR_SIZE, S_PG_LVARCHAR_SIZE,
      S_PG_DEBUG, S_PG_COMM_LOG, S_PG_OPTIMIZER, S_PG_KSQO, S_PG_DECLARE_FETCH,
      S_PG_TEXT_LVARCHAR, S_PG_UNKNOWNS_LVARCHAR, S_PG_BOOLS_AS_CHAR,
      S_PG_PARSE, S_PG_CANCEL_FREE_STMT, S_PG_SYS_TABLE_PREFIXES,
      S_PG_LF_CONVERSION, S_PG_UPD_CURSORS, S_PG_DISALLOW_PREMATURE,
      S_PG_TRUE_IS_MINUS1, S_PG_BI, S_PG_BYTEA_LVARBINARY,
      S_PG_SERVERSIDE_PREPARE, S_PG_LOWERCASE_ID, S_PG_GSSAUTH_USE_GSS,
      S_PG_XA_OPT, S_PG_INIT_CATALOG, PgDatabase]);
end;

//---------------------------------------------------------------------------

function TdmData.GetFileConnectionString(const FileName: AnsiString):
  AnsiString;
begin
  //  if AnsiUpperCase(ExtractFileExt(FileName)) = '.XLSX' then
  Result := Format(S_XLSX_FORMAT, [S_EXCEL_PROVIDER12, S_EXCEL_USERID,
    S_EXCEL_DATASOURCE, FileName, S_EXCEL_MODE, S_EXCEL_EXTENDED12,
      S_EXCEL_JET_DB, S_EXCEL_JET_REG, S_EXCEL_JET_PASS, S_EXCEL_JET_ENGINE,
      S_EXCEL_JET_LOCK, S_EXCEL_JET_BULK_OPS, S_EXCEL_JET_BULK_TRN,
      S_EXCEL_JET_NEWPASS, S_EXCEL_JET_CRTDB, S_EXCEL_JET_ENC,
      S_EXCEL_JET_COPY, S_EXCEL_JET_REPL, S_EXCEL_JET_SFP,
      S_EXCEL_JET_COMPLEX]);
  {  else
      Result := Format(S_XLS_FORMAT, [S_EXCEL_PROVIDER4, S_EXCEL_DATASOURCE,
        FileName, S_EXCEL_EXTENDED4]);
        }
end;

//---------------------------------------------------------------------------

function TdmData.GetFileRecordsCount: Integer;
begin
  Result := qryFileSelect.RecordCount;
end;

//---------------------------------------------------------------------------

function TdmData.GetForestryByRegion(const RegionID: Integer): TStringList;
var
  AWord: AnsiString;

begin
  Result := TStringList.Create;
  Result.Clear();

  try
    Connect();
    qryGetTableValues.SQL.Text := Format(S_SQL_GET_FORESTRIES_BY_REGION,
      [RegionID]);
    qryGetTableValues.Open();

    qryGetTableValues.First();
    while not qryGetTableValues.Eof do
    begin
      AWord := AnsiUpperCase(Trim(qryGetTableValues.Fields[0].AsString));
      Result.Add(AWord);
      qryGetTableValues.Next();
    end;
    qryGetTableValues.Close();

  finally
    qryGetTableValues.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.GetInProgress: Boolean;
begin
  Result := FInProgress;
end;

//---------------------------------------------------------------------------

function TdmData.GetIntField(SQLText: AnsiString): Integer;
begin
  try
    Connect();
    qryGetTableValues.SQL.Text := SQLText;
    qryGetTableValues.Open();
    Result := qryGetTableValues.Fields[0].AsInteger;

  finally
    qryGetTableValues.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.GetLogDetails: TLogDetails;
begin
  Result := FLogDetails;
end;

//---------------------------------------------------------------------------

function TdmData.GetPrevReportSums(const ForestryID, ReportYear, ReportQuarter:
  Integer; const PrevYear: Boolean): TReportSums;
begin
  try
    Connect();

    if PrevYear then
      qryGetTableValues.SQL.Text := Format(S_DB_GET_PREV_YEAR_SUMS,
        [S_DB_TABLE_NAME, ForestryID, ReportYear])
    else
      qryGetTableValues.SQL.Text := Format(S_DB_GET_PREV_QUARTER_SUMS,
        [S_DB_TABLE_NAME, ForestryID, ReportYear, ReportQuarter]);

    qryGetTableValues.Open();
    Result.DamagedArea := qryGetTableValues.Fields[0].AsCurrency;
    Result.LostArea := qryGetTableValues.Fields[1].AsCurrency;
    Result.PestArea := qryGetTableValues.Fields[2].AsCurrency;

  finally
    qryGetTableValues.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.GetQueryRecordsCount: Integer;
begin
  Result := mtCache.RecordCount;
end;

//---------------------------------------------------------------------------

procedure TdmData.GetQueryResult(const SQLText: AnsiString);
begin
  if mtCache.Active then
    mtCache.Close();

  try
    try
      Connect();
      qrySelect.SelectSQL.Text := SQLText;
      mtCache.Open();

    except
      ShowMsg(S_QUERY_EXEC_SUCCESS);
      raise;
    end;

  finally
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.GetScript: AnsiString;
begin
  Result := FScript.GetText();
end;

//---------------------------------------------------------------------------

function TdmData.GetTableList: TStringList;
begin
  Result := FTableList;
end;

//---------------------------------------------------------------------------

function TdmData.GetValuesFromTable(const DictCaption: AnsiString): TDictArr;
var
  I: Integer;

begin
  SetLength(Result, 0);

  try
    Connect();
    qryGetTableValues.SQL.Text := FValidator.GetDictionary(DictCaption).SQLScript;
    qryGetTableValues.Open();

    qryGetTableValues.First();
    while not qryGetTableValues.Eof do
    begin
      I := Length(Result);
      SetLength(Result, I + 1);
      Result[I].WordIndex := qryGetTableValues.Fields[0].AsInteger;
      Result[I].WordValue :=
        AnsiUpperCase(Trim(qryGetTableValues.Fields[1].AsString));
      qryGetTableValues.Next();
    end;

  finally
    qryGetTableValues.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.InvalidRow(RecNo: Integer): Boolean;
var
  I: Integer;

begin
  Result := False;

  for I := 0 to Length(FInvalidRecs) - 1 do
    if FInvalidRecs[I] = RecNo then
    begin
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------

procedure TdmData.Log(S: string);
var
  F: TextFile;

begin
  if not FileExists('log.log') then
  begin
    AssignFile(F, 'log.log');
    Rewrite(F);
    CloseFile(F);
  end;

  S := Format('%s %s: %s', [DateTimeToStr(Date()), DateTimeToStr(Time()), S]);
  AssignFile(F, 'log.log');
  Append(F);
  WriteLn(F, S);
  CloseFile(F);
end;

//---------------------------------------------------------------------------

procedure TdmData.MoveNextSkipped;
var
  I: Integer;

begin
  if Length(FSkippedRecs) < 1 then
    Exit;

  for I := 0 to Length(FSkippedRecs) - 1 do
  begin
    if FSkippedRecs[I] > qryFileSelect.RecNo then
    begin
      MoveTo(qryFileSelect, SkippedRecs[I]);
      Exit;
    end;
  end;

  MoveTo(qryFileSelect, SkippedRecs[0]);
end;

//---------------------------------------------------------------------------

procedure TdmData.MovePrevSkipped;
var
  I: Integer;

begin
  if Length(FSkippedRecs) < 1 then
    Exit;

  for I := Length(FSkippedRecs) - 1 downto 0 do
  begin
    if FSkippedRecs[I] < qryFileSelect.RecNo then
    begin
      MoveTo(qryFileSelect, SkippedRecs[I]);
      Exit;
    end;
  end;

  MoveTo(qryFileSelect, SkippedRecs[Length(FSkippedRecs) - 1]);
end;

//---------------------------------------------------------------------------

function TdmData.MoveTo(const DataSet: TDataSet; const Position: Integer):
  Boolean;
begin
  DataSet.First();
  Result := DataSet.MoveBy(Position - 1) = (Position - 1);
end;

//---------------------------------------------------------------------------

function TdmData.OpenFile(const FileName: AnsiString): Boolean;
var
  I: Integer;

begin
  FFirstRow := 0;
  FLastRow := MaxInt;
  FScript.Clear();
  SetLength(FSkippedRecs, 0);
  SetLength(FInvalidRecs, 0);

  if qryFileSelect.Active then
    qryFileSelect.Close();
  if connFile.Connected then
    connFile.Close();

  connFile.ConnectionString := GetFileConnectionString(FileName);
  try
    connFile.Open();
    connFile.GetTableNames(FTableList);

    for I := FTableList.Count - 1 downto 0 do
    begin
      if AnsiPos('#_FilterDatabase', FTableList.Strings[I]) > 0 then
        FTableList.Delete(I);
      if AnsiPos('#Print_Area', FTableList.Strings[I]) > 0 then
        FTableList.Delete(I);
    end;
    Result := connFile.Connected;

  except
    raise;
  end;
end;

//---------------------------------------------------------------------------

function TdmData.OpenTable(const TableName: AnsiString): Boolean;
begin
  FFirstRow := 0;
  FLastRow := MaxInt;
  FScript.Clear();
  SetLength(FSkippedRecs, 0);
  SetLength(FInvalidRecs, 0);

  qryFileSelect.SQL.Text := Format(S_FILE_SELECT, [TableName]);

  try
    qryFileSelect.Open();
    Result := qryFileSelect.Active;

  except
    raise;
  end;
end;

//---------------------------------------------------------------------------

procedure TdmData.PositionTable;
begin
  try
    if not (FindFirstRow() and FindLastRow()) then
      raise Exception.Create(E_FIND_FIRST_CELL);

  except
    raise Exception.Create(E_FIND_FIRST_CELL);
  end;
end;

//---------------------------------------------------------------------------

procedure TdmData.Progress(CurrentIteration: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(CurrentIteration);
end;

//---------------------------------------------------------------------------

procedure TdmData.ReadDBString(const RegionID, ForestryID, ReportQuarter,
  ReportYear: Integer);
begin
  FCurrentRecord.RegionID := RegionID;
  FCurrentRecord.ForestryName := Trim(qryFileSelect.Fields[0 +
    FFirstCol].AsString);
  FCurrentRecord.ForestryID := ForestryID;
  FCurrentRecord.LocalForestryName := Trim(qryFileSelect.Fields[1 +
    FFirstCol].AsString);
  FCurrentRecord.Quarter := DefInteger(qryFileSelect.Fields[2 +
    FFirstCol].AsVariant);
  FCurrentRecord.Patch := DefInteger(qryFileSelect.Fields[3 +
    FFirstCol].AsVariant);
  FCurrentRecord.LanduseName := Trim(qryFileSelect.Fields[4 +
    FFirstCol].AsString);
  FCurrentRecord.DefenseCategoryName := Trim(qryFileSelect.Fields[5 +
    FFirstCol].AsString);
  FCurrentRecord.MainSpeciesName := Trim(qryFileSelect.Fields[6 +
    FFirstCol].AsString);
  FCurrentRecord.DamageSpeciesName := Trim(qryFileSelect.Fields[7 +
    FFirstCol].AsString);
  FCurrentRecord.DamageReasonName := Trim(qryFileSelect.Fields[8 +
    FFirstCol].AsString);
  FCurrentRecord.F10 := DefInteger(qryFileSelect.Fields[9 +
    FFirstCol].AsVariant);
  FCurrentRecord.F11 := DefInteger(qryFileSelect.Fields[10 +
    FFirstCol].AsVariant);
  FCurrentRecord.F12 := DefCurrency(qryFileSelect.Fields[11 +
    FFirstCol].AsVariant);
  FCurrentRecord.F13 := DefCurrency(qryFileSelect.Fields[12 +
    FFirstCol].AsVariant);
  FCurrentRecord.F14 := DefCurrency(qryFileSelect.Fields[13 +
    FFirstCol].AsVariant);
  FCurrentRecord.F15 := DefCurrency(qryFileSelect.Fields[14 +
    FFirstCol].AsVariant);
  FCurrentRecord.F16 := DefCurrency(qryFileSelect.Fields[15 +
    FFirstCol].AsVariant);
  FCurrentRecord.F17 := DefCurrency(qryFileSelect.Fields[16 +
    FFirstCol].AsVariant);
  FCurrentRecord.F18 := DefCurrency(qryFileSelect.Fields[17 +
    FFirstCol].AsVariant);
  FCurrentRecord.F19 := DefCurrency(qryFileSelect.Fields[18 +
    FFirstCol].AsVariant);
  FCurrentRecord.F20 := DefCurrency(qryFileSelect.Fields[19 +
    FFirstCol].AsVariant);
  FCurrentRecord.F21 := DefCurrency(qryFileSelect.Fields[20 +
    FFirstCol].AsVariant);
  FCurrentRecord.F22 := DefCurrency(qryFileSelect.Fields[21 +
    FFirstCol].AsVariant);
  FCurrentRecord.F23 := DefCurrency(qryFileSelect.Fields[22 +
    FFirstCol].AsVariant);
  FCurrentRecord.F24 := DefCurrency(qryFileSelect.Fields[23 +
    FFirstCol].AsVariant);
  FCurrentRecord.F25 := DefCurrency(qryFileSelect.Fields[24 +
    FFirstCol].AsVariant);
  FCurrentRecord.F26 := DefCurrency(qryFileSelect.Fields[25 +
    FFirstCol].AsVariant);
  FCurrentRecord.F27 := DefCurrency(qryFileSelect.Fields[26 +
    FFirstCol].AsVariant);
  FCurrentRecord.F28 := DefCurrency(qryFileSelect.Fields[27 +
    FFirstCol].AsVariant);
  FCurrentRecord.F29 := DefCurrency(qryFileSelect.Fields[28 +
    FFirstCol].AsVariant);
  FCurrentRecord.F30 := Trim(qryFileSelect.Fields[29 + FFirstCol].AsString);
  FCurrentRecord.F31 := DefCurrency(qryFileSelect.Fields[30 +
    FFirstCol].AsVariant);
  FCurrentRecord.F32 := DefCurrency(qryFileSelect.Fields[31 +
    FFirstCol].AsVariant);
  FCurrentRecord.F33 := DefCurrency(qryFileSelect.Fields[32 +
    FFirstCol].AsVariant);
  FCurrentRecord.F34 := DefCurrency(qryFileSelect.Fields[33 +
    FFirstCol].AsVariant);
  FCurrentRecord.F35 := DefCurrency(qryFileSelect.Fields[34 +
    FFirstCol].AsVariant);
  FCurrentRecord.F36 := DefCurrency(qryFileSelect.Fields[35 +
    FFirstCol].AsVariant);
  FCurrentRecord.F37 := DefCurrency(qryFileSelect.Fields[36 +
    FFirstCol].AsVariant);
  FCurrentRecord.F38 := DefCurrency(qryFileSelect.Fields[37 +
    FFirstCol].AsVariant);
  FCurrentRecord.F39 := DefCurrency(qryFileSelect.Fields[38 +
    FFirstCol].AsVariant);
  FCurrentRecord.F40 := DefCurrency(qryFileSelect.Fields[39 +
    FFirstCol].AsVariant);
  FCurrentRecord.F41 := DefCurrency(qryFileSelect.Fields[40 +
    FFirstCol].AsVariant);
  FCurrentRecord.F42 := DefCurrency(qryFileSelect.Fields[41 +
    FFirstCol].AsVariant);
  FCurrentRecord.F43 := DefCurrency(qryFileSelect.Fields[42 +
    FFirstCol].AsVariant);
  FCurrentRecord.F44 := DefCurrency(qryFileSelect.Fields[43 +
    FFirstCol].AsVariant);
  FCurrentRecord.F45 := DefCurrency(qryFileSelect.Fields[44 +
    FFirstCol].AsVariant);
  FCurrentRecord.F46 := DefCurrency(qryFileSelect.Fields[45 +
    FFirstCol].AsVariant);
  FCurrentRecord.F47 := DefCurrency(qryFileSelect.Fields[46 +
    FFirstCol].AsVariant);
  FCurrentRecord.F48 := DefCurrency(qryFileSelect.Fields[47 +
    FFirstCol].AsVariant);
  FCurrentRecord.F49 := DefCurrency(qryFileSelect.Fields[48 +
    FFirstCol].AsVariant);
  FCurrentRecord.F50 := Trim(qryFileSelect.Fields[49 + FFirstCol].AsString);
  FCurrentRecord.F51 := DefCurrency(qryFileSelect.Fields[50 +
    FFirstCol].AsVariant);
  FCurrentRecord.F52 := DefCurrency(qryFileSelect.Fields[51 +
    FFirstCol].AsVariant);
  FCurrentRecord.F53 := DefCurrency(qryFileSelect.Fields[52 +
    FFirstCol].AsVariant);
  FCurrentRecord.F54 := DefCurrency(qryFileSelect.Fields[53 +
    FFirstCol].AsVariant);
  FCurrentRecord.F55 := DefCurrency(qryFileSelect.Fields[54 +
    FFirstCol].AsVariant);
  FCurrentRecord.F56 := DefCurrency(qryFileSelect.Fields[55 +
    FFirstCol].AsVariant);
  FCurrentRecord.PestStatus := Trim(qryFileSelect.Fields[56 +
    FFirstCol].AsString);
  FCurrentRecord.PestName := Trim(qryFileSelect.Fields[57 +
    FFirstCol].AsString);
  FCurrentRecord.F59 := DefCurrency(qryFileSelect.Fields[58 +
    FFirstCol].AsVariant);
  FCurrentRecord.F60 := DefCurrency(qryFileSelect.Fields[59 +
    FFirstCol].AsVariant);
  FCurrentRecord.F61 := DefCurrency(qryFileSelect.Fields[60 +
    FFirstCol].AsVariant);
  FCurrentRecord.F62 := DefCurrency(qryFileSelect.Fields[61 +
    FFirstCol].AsVariant);
  FCurrentRecord.F63 := DefCurrency(qryFileSelect.Fields[62 +
    FFirstCol].AsVariant);
  FCurrentRecord.F64 := DefCurrency(qryFileSelect.Fields[63 +
    FFirstCol].AsVariant);
  FCurrentRecord.F65 := DefCurrency(qryFileSelect.Fields[64 +
    FFirstCol].AsVariant);
  FCurrentRecord.F66 := DefCurrency(qryFileSelect.Fields[65 +
    FFirstCol].AsVariant);
  FCurrentRecord.F67 := DefCurrency(qryFileSelect.Fields[66 +
    FFirstCol].AsVariant);
  FCurrentRecord.F68 := DefCurrency(qryFileSelect.Fields[67 +
    FFirstCol].AsVariant);
  FCurrentRecord.ReportQuarter := ReportQuarter;
  FCurrentRecord.ReportYear := ReportYear;
  FCurrentRecord.ValidationResult := [];
end;

//---------------------------------------------------------------------------

procedure TdmData.ReadSettings;
begin
  if not FileExists(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME)
    then
    Exit;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      PgDatabase := ReadString(S_INI_DATA, S_DB_DATABASE, PgDatabase);
      PgServer := ReadString(S_INI_DATA, S_DB_SERVER, PgServer);
      PgPort := ReadString(S_INI_DATA, S_DB_PORT, PgPort);
      PgUID := ReadString(S_INI_DATA, S_DB_UID, PgUID);
      PgPassword := ReadString(S_INI_DATA, S_DB_PASSWORD, PgPassword);

    finally
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TdmData.RemoveSkippedRec(RecNo: Integer);
var
  I: Integer;

begin
  for I := 0 to Length(FSkippedRecs) - 2 do // 2!!!
  begin
    if FSkippedRecs[I] >= RecNo then
      FSkippedRecs[I] := FSkippedRecs[I + 1];
  end;

  if FSkippedRecs[Length(FSkippedRecs) - 1] >= RecNo then
    SetLength(FSkippedRecs, Length(FSkippedRecs) - 1);
end;

//---------------------------------------------------------------------------

procedure TdmData.Rollback;
begin
  connDB.RollbackTrans();
end;

//---------------------------------------------------------------------------

function TdmData.SearchForDuplicates: TValidationRes;
var
  CurRecNo: Integer;
  UniqueKey: TUniqueKey;

begin
  qryFileSelect.DisableControls();
  CurRecNo := FFirstRow;
  MoveTo(qryFileSelect, CurRecNo);
  Result := vrOk;

  while CurRecNo < FLastRow do
  begin
    UniqueKey.RecNo := qryFileSelect.RecNo;
    UniqueKey.Forestry := Trim(qryFileSelect.Fields[0 + FFirstCol].AsString);
    UniqueKey.LocalForestry := Trim(qryFileSelect.Fields[1 +
      FFirstCol].AsString);
    UniqueKey.Quarter := DefInteger(qryFileSelect.Fields[2 +
      FFirstCol].AsVariant);
    UniqueKey.Location := DefInteger(qryFileSelect.Fields[3 +
      FFirstCol].AsVariant);
    UniqueKey.Landuse := Trim(qryFileSelect.Fields[4 + FFirstCol].AsString);
    UniqueKey.LocationArea := DefCurrency(qryFileSelect.Fields[11 +
      FFirstCol].AsVariant);

    qryFileSelect.Next();

    while qryFileSelect.RecNo <= FLastRow do
    begin
      if (UniqueKey.Forestry = Trim(qryFileSelect.Fields[0 +
        FFirstCol].AsString)) and
        (UniqueKey.LocalForestry = Trim(qryFileSelect.Fields[1 +
          FFirstCol].AsString)) and
        (UniqueKey.Quarter = DefInteger(qryFileSelect.Fields[2 +
          FFirstCol].AsVariant)) and
        (UniqueKey.Location = DefInteger(qryFileSelect.Fields[3 +
          FFirstCol].AsVariant)) and
        (UniqueKey.Landuse = Trim(qryFileSelect.Fields[4 + FFirstCol].AsString))
          and
        (UniqueKey.LocationArea = DefCurrency(qryFileSelect.Fields[11 +
          FFirstCol].AsVariant)) then
      begin
        if ldDuplicates in FLogDetails then
          ValidationLog(Format(S_LOG_DUPLICATE_ROW, [UniqueKey.RecNo,
            qryFileSelect.RecNo]));
        Result := vrDuplicateInvalid;
      end;
      qryFileSelect.Next();
    end;

    Inc(CurRecNo);
    MoveTo(qryFileSelect, CurRecNo);
  end;

  qryFileSelect.EnableControls();
end;

//---------------------------------------------------------------------------

procedure TdmData.SetLogDetails(const Value: TLogDetails);
begin
  if FLogDetails <> Value then
    FLogDetails := Value;
end;

//---------------------------------------------------------------------------

function TdmData.SkippedRow(RecNo: Integer): Boolean;
var
  I: Integer;

begin
  Result := False;

  for I := 0 to Length(FSkippedRecs) - 1 do
    if FSkippedRecs[I] = RecNo then
    begin
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------

procedure TdmData.ValidateRecord(const RegionID, ForestryID, ReportYear,
  ReportQuarter: Integer);
begin
  FValidator.InitCheck(FLogDetails);

  if EmptyRec() then
  begin
    if ldEmptyRecords in FLogDetails then
      ValidationLog(Format(S_LOG_EMPTY_ROW, [qryFileSelect.RecNo]));
    Exit;
  end;

  ReadDBString(ForestryID, RegionID, ReportQuarter, ReportYear);

  FValidator.Validate(qryFileSelect.RecNo, FCurrentRecord);
  FCurrentRecord.ValidationResult := FValidator.ValidationResult;

  if (vrStop in FCurrentRecord.ValidationResult) or (vrSkip in
    FCurrentRecord.ValidationResult) then
    Exit;

  if ((vrStringInvalid in FCurrentRecord.ValidationResult)
    and (ldDictReplaces in FLogDetails))
    or ((vrRelationInvalid in FCurrentRecord.ValidationResult)
    and (ldRelationErrors in FLogDetails))
    or ((vrMainInvalid in FCurrentRecord.ValidationResult)
    and (ldMathErrors in FLogDetails))
    or ((vrExtraInvalid in FCurrentRecord.ValidationResult)
    and (ldMathErrors in FLogDetails)) then
    ValidationLog(FValidator.RecordStatus);

  RemoveSkippedRec(qryFileSelect.RecNo);
end;

//---------------------------------------------------------------------------

procedure TdmData.ValidateTable(const RegionID, ForestryID, ReportYear,
  ReportQuarter: Integer);
var
  CurRecNo: Integer;
  CurReportSums, PrevReportSums: TReportSums;

begin
  FInProgress := True;
  FValidationResult := [];
  SetLength(FSkippedRecs, 0);

  CurReportSums.DamagedArea := 0;
  CurReportSums.LostArea := 0;
  CurReportSums.PestArea := 0;

  FScript.Clear();
  FScript.SetScriptHeader();
  if ExistsSameReport(ForestryID, ReportQuarter, ReportYear) then
    FScript.AddDelete(RegionID, ForestryID, ReportQuarter, ReportYear);

  FValidator.InitCheck(FLogDetails);

  try
    Include(FValidationResult, SearchForDuplicates());

    for CurRecNo := FFirstRow to FLastRow do
    begin
      Application.ProcessMessages();
      MoveTo(qryFileSelect, CurRecNo);
      Progress(qryFileSelect.RecNo);

      if EmptyRec() then
      begin
        if ldEmptyRecords in FLogDetails then
          ValidationLog(Format(S_LOG_EMPTY_ROW, [qryFileSelect.RecNo]));
        Continue;
      end;

      ReadDBString(ForestryID, RegionID, ReportQuarter, ReportYear);

      FValidator.Validate(qryFileSelect.RecNo, FCurrentRecord);
      FValidationResult := FValidationResult + FCurrentRecord.ValidationResult;

      CurReportSums.DamagedArea := CurReportSums.DamagedArea +
        FCurrentRecord.F17;
      CurReportSums.LostArea := CurReportSums.LostArea + FCurrentRecord.F24;
      CurReportSums.PestArea := CurReportSums.PestArea + FCurrentRecord.F59;

      if vrStop in FCurrentRecord.ValidationResult then
      begin
        ValidationLog(S_LOG_FORCE_STOP);
        Break;
      end;

      if vrSkip in FCurrentRecord.ValidationResult then
        AddSkippedRec(CurRecNo);

      if ((vrStringInvalid in FCurrentRecord.ValidationResult)
        and (ldDictReplaces in FLogDetails))
        or ((vrRelationInvalid in FCurrentRecord.ValidationResult)
        and (ldRelationErrors in FLogDetails))
        or ((vrMainInvalid in FCurrentRecord.ValidationResult)
        and (ldMathErrors in FLogDetails))
        or ((vrExtraInvalid in FCurrentRecord.ValidationResult)
        and (ldMathErrors in FLogDetails)) then
      begin
        ValidationLog(FValidator.RecordStatus);
        AddInvalidRec(CurRecNo);
      end;

      FScript.AddInsert(FCurrentRecord);
    end;

    case ReportQuarter of
      1:
        PrevReportSums := GetPrevReportSums(ForestryID, ReportYear,
          ReportQuarter, True);
    else
      PrevReportSums := GetPrevReportSums(ForestryID, ReportYear, ReportQuarter,
        False);
    end;

    if ldPrevReportSum in FLogDetails then
      if ExistsPrevReport(ForestryID, ReportQuarter, ReportYear) then
      begin
        if CurReportSums.DamagedArea <> PrevReportSums.DamagedArea then
          ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [17]));

        if CurReportSums.LostArea <> PrevReportSums.LostArea then
          ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [24]));

        if CurReportSums.PestArea <> PrevReportSums.PestArea then
          ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [59]));
      end
      else
        ValidationLog(S_LOG_PREV_REPORT_NOT_FOUND);

    FScript.SetScriptFooter();
    ValidationLog(DateToStr(Date()) + ' ' + TimeToStr(Time()) + ' ' +
      S_LOG_COMPLETED);

  finally
    FInProgress := False;
  end;
end;

//---------------------------------------------------------------------------

procedure TdmData.ValidationLog(Str: AnsiString);
begin
  if Assigned(FOnValidationLog) then
    FOnValidationLog(Str);
end;

//---------------------------------------------------------------------------

procedure TdmData.WriteSettings;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      WriteString(S_INI_DATA, S_DB_DATABASE, PgDatabase);
      WriteString(S_INI_DATA, S_DB_SERVER, PgServer);
      WriteString(S_INI_DATA, S_DB_PORT, PgPort);
      WriteString(S_INI_DATA, S_DB_UID, PgUID);
      WriteString(S_INI_DATA, S_DB_PASSWORD, PgPassword);

    finally
      Free();
    end;
end;

end.

