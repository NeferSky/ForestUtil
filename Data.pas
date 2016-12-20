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
    FContinueOnError: Boolean;
    FFirstRow: Integer;
    FLastRow: Integer;
    FFirstCol: Integer;
    FTableList: TStringList;
    FScript: TDBScript;
    FOnValidationLog: TOnValidationLog;
    FOnProgress: TOnProgress;
    procedure ReadSettings;
    procedure WriteSettings;
    procedure SearchForDuplicates;
    procedure Connect;
    procedure Disconnect;
    procedure Commit;
    procedure Rollback;
    procedure ReadDBString(var Values: TValuesRec);
    function GetDBConnectionString: AnsiString;
    function GetFileConnectionString(const FileName: AnsiString): AnsiString;
    function GetSQLByDictName(const DictName: AnsiString): AnsiString;
    function FindFirstRow: Boolean;
    function FindLastRow: Boolean;
    function EmptyRec: Boolean;
    function GetTableList: TStringList;
    function GetInProgress: Boolean;
    function GetContinueOnError: Boolean;
    procedure SetContinueOnError(Value: Boolean);
    function SetPosition(const DataSet: TDataSet; const Position: Integer): Boolean;
    function ExistsSameReport(const ForestryID, ReportQuarter,
      ReportYear: Integer): Boolean;
    procedure ValidationLog(Msg: AnsiString);
    procedure Progress(CurrentIteration: Integer);
    function GetPrevReportSums(const ForestryID, ReportYear,
      ReportQuarter: Integer; const PrevYear: Boolean): TReportSums;
  public
    procedure Log(S: string);
    { Public declarations }
    function GetValidList(Dictionary: AnsiString): TStringList;
    procedure SetValidList(Dictionary: AnsiString; Value: TStringList);
    function GetValuesFromTable(const DictName: AnsiString): TStringList;
    function GetForestry(const RegionID: Integer): TStringList;
    function GetLocalForestry(const ForestryName: AnsiString): TStringList;
    function GetIntField(SQLText: AnsiString): Integer;
    function CheckSpeciesRelation(const SpeciesName,
      CauseName: AnsiString): Boolean;
    //
    procedure ExecuteQuery(const SQLText: AnsiString);
    procedure GetQueryResult(const SQLText: AnsiString);
    function GetQueryRecordsCount: Integer;
    //
    function OpenFile(const FileName: AnsiString): Boolean;
    function OpenTable(const TableName: AnsiString): Boolean;
    function GetFileRecordsCount: Integer;
    function MathValidateFile(const ForestryID, ReportYear,
      ReportQuarter: Integer): TValidationResult;
    function StringValidateFile(const RegionID, ForestryID, ReportQuarter,
      ReportYear: Integer): TValidationResult;
    procedure PositionTable;
    function GetResultScript: AnsiString;
    //
    property TableList: TStringList read GetTableList;
    property ContinueOnError: Boolean read GetContinueOnError write
      SetContinueOnError;
    property InProgress: Boolean read GetInProgress;
    property OnValidationLog: TOnValidationLog read FOnValidationLog
      write FOnValidationLog;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

var
  dmData: TdmData;
  vld: TValidator;

implementation

uses
  ForestConsts, IniFiles, Forms, NsUtils;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TdmData }

function TdmData.CheckSpeciesRelation(const SpeciesName,
  CauseName: AnsiString): Boolean;
begin
  Result := GetIntField(Format(S_DB_GET_SPECIES_RELATION,
    [SpeciesName, CauseName])) > 0
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
  FContinueOnError := True;
  FTableList := TStringList.Create();
  FScript := TDBScript.Create();
  ReadSettings();
  vld := TValidator.Create(Application);
end;

//---------------------------------------------------------------------------

procedure TdmData.DataModuleDestroy(Sender: TObject);
begin
  WriteSettings();
  FTableList.Free();
  FScript.Free();
  vld.Free();
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
    else
      if ((I = 2) or (I = 3) or (I = 9) or (I = 10)) then
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
      MessageBox(Application.Handle, PChar(S_QUERY_EXEC_SUCCESS), PChar(''), 0);
    except
      Rollback();
      MessageBox(Application.Handle, PChar(E_QUERY_EXEC_ERROR), PChar(''), 0);
    end;
  finally
    qryCommand.Close();
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.ExistsSameReport(const ForestryID, ReportQuarter,
  ReportYear: Integer): Boolean;
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

function TdmData.GetContinueOnError: Boolean;
begin
  Result := FContinueOnError;
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

function TdmData.GetFileConnectionString(
  const FileName: AnsiString): AnsiString;
begin
  //  if UpperCase(ExtractFileExt(FileName)) = '.XLSX' then
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

function TdmData.GetForestry(const RegionID: Integer): TStringList;
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

function TdmData.GetLocalForestry(const ForestryName: AnsiString): TStringList;
var
  ForestryID: Integer;
  AWord: AnsiString;

begin
  Result := TStringList.Create;
  Result.Clear();

  try
    ForestryID := GetIntField(Format(S_DB_GET_FORESTRY_ID, [ForestryName]));

    Connect();
    qryGetTableValues.SQL.Text := Format(S_SQL_GET_LOCAL_FORESTRIES_BY_FORESTRY,
      [ForestryID]);
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

function TdmData.GetPrevReportSums(const ForestryID, ReportYear,
  ReportQuarter: Integer; const PrevYear: Boolean): TReportSums;
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
      MessageBox(Application.Handle, PChar(S_QUERY_EXEC_SUCCESS), PChar(''), 0);
      raise;
    end;
  finally
    Disconnect();
  end;
end;

//---------------------------------------------------------------------------

function TdmData.GetResultScript: AnsiString;
begin
  Result := FScript.GetText();
end;

//---------------------------------------------------------------------------

function TdmData.GetSQLByDictName(const DictName: AnsiString): AnsiString;
begin
  if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_FORESTRIES_FILE then
    Result := S_SQL_GET_FORESTRIES_DICT

  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LOCAL_FORESTRIES_FILE then
    Result := S_SQL_GET_LOCAL_FORESTRIES_DICT

  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LANDUSE_FILE then
    Result := S_SQL_GET_LANDUSE_DICT
      
  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PROTECT_CATEGORY_FILE then
    Result := S_SQL_GET_PROTECT_CATEGORY_DICT

  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_SPECIES_FILE then
    Result := S_SQL_GET_SPECIES_DICT

  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_DAMAGE_FILE then
    Result := S_SQL_GET_DAMAGE_DICT

  else if DictName = S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PEST_FILE then
    Result := S_SQL_GET_PEST_DICT;
end;

//---------------------------------------------------------------------------

function TdmData.GetTableList: TStringList;
begin
  Result := FTableList;
end;

//---------------------------------------------------------------------------

function TdmData.GetValidList(Dictionary: AnsiString): TStringList;
begin
  Result := vld.GetValidList(Dictionary);
end;

//---------------------------------------------------------------------------

function TdmData.GetValuesFromTable(const DictName: AnsiString): TStringList;
var
  AWord: AnsiString;

begin
  Result := TStringList.Create;
  Result.Clear();

  try
    Connect();
    qryGetTableValues.SQL.Text := GetSQLByDictName(DictName);
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

function TdmData.MathValidateFile(const ForestryID, ReportYear,
  ReportQuarter: Integer): TValidationResult;
var
  RecStatus: string;
  Values: TValuesRec;
  CurRec: Integer;
  ValRes: TValidationResult;
  CurReportSums, PrevReportSums: TReportSums;

begin
  FInProgress := True;

  Result := [vrDuplicateValid, vrMainValid, vrExtraValid, vrStringValid];
  CurReportSums.DamagedArea := 0;
  CurReportSums.LostArea := 0;
  CurReportSums.PestArea := 0;

  try
    qryFileSelect.DisableControls();
    Result := [];

    FScript.Clear();
    FScript.SetScriptHeader();

{$IFNDEF DEBUG}
    SearchForDuplicates();
{$ENDIF}
    qryFileSelect.EnableControls();

    for CurRec := FFirstRow to FLastRow do
    begin
      Application.ProcessMessages();
      SetPosition(qryFileSelect, CurRec);
      Progress(qryFileSelect.RecNo);

      if EmptyRec() then
      begin
        ValidationLog(S_LOG_EMPTY_ROW + IntToStr(qryFileSelect.RecNo));
        Continue;
      end;

      ReadDBString(Values);
      ValRes := vld.MathValidateRecord(qryFileSelect.RecNo, Values, RecStatus);

      CurReportSums.DamagedArea := CurReportSums.DamagedArea + Values.F17;
      CurReportSums.LostArea := CurReportSums.LostArea + Values.F24;
      CurReportSums.PestArea := CurReportSums.PestArea + Values.F59;

      if (vrDuplicateInvalid in ValRes) or (vrMainInvalid in ValRes) or
        (vrExtraInvalid in ValRes) or (vrStringInvalid in ValRes) then
        begin
          if Assigned(FOnValidationLog) then
            FOnValidationLog(RecStatus);
          Result := Result + ValRes;
        end;

      if (vrMainInvalid in ValRes) and not FContinueOnError then
        Break;
    end;

    case ReportQuarter of
    1:
      begin
        PrevReportSums := GetPrevReportSums(ForestryID, ReportYear,
          ReportQuarter, True);
      end;

    2, 3, 4:
      begin
        PrevReportSums := GetPrevReportSums(ForestryID, ReportYear,
          ReportQuarter, True);
      end;
    end;

    if CurReportSums.DamagedArea <> PrevReportSums.DamagedArea then
      ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [17]));

    if CurReportSums.LostArea <> PrevReportSums.LostArea then
      ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [24]));

    if CurReportSums.PestArea <> PrevReportSums.PestArea then
      ValidationLog(Format(S_LOG_INVALID_SUM_PREV_REPORT, [59]));

    ValidationLog(#13#10 + DateToStr(Date()) + ' ' + TimeToStr(Time()) + ' ' +
      S_LOG_COMPLETED);

  finally
    qryFileSelect.EnableControls();
    FInProgress := False;
  end
end;

//---------------------------------------------------------------------------

function TdmData.OpenFile(const FileName: AnsiString): Boolean;
var
  I: Integer;

begin
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
      if AnsiPos('_xlnm#Print_Area', FTableList.Strings[I]) > 0 then
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

procedure TdmData.ReadDBString(var Values: TValuesRec);
begin
  Values.F1 := Trim(qryFileSelect.Fields[0 + FFirstCol].AsString);
  Values.F2 := Trim(qryFileSelect.Fields[1 + FFirstCol].AsString);
  Values.F3 := DefInteger(qryFileSelect.Fields[2 + FFirstCol].AsVariant);
  Values.F4 := DefInteger(qryFileSelect.Fields[3 + FFirstCol].AsVariant);
  Values.F5 := Trim(qryFileSelect.Fields[4 + FFirstCol].AsString);
  Values.F6 := Trim(qryFileSelect.Fields[5 + FFirstCol].AsString);
  Values.F7 := Trim(qryFileSelect.Fields[6 + FFirstCol].AsString);
  Values.F8 := Trim(qryFileSelect.Fields[7 + FFirstCol].AsString);
  Values.F9 := Trim(qryFileSelect.Fields[8 + FFirstCol].AsString);
  Values.F10 := DefInteger(qryFileSelect.Fields[9 + FFirstCol].AsVariant);
  Values.F11 := DefInteger(qryFileSelect.Fields[10 + FFirstCol].AsVariant);
  Values.F12 := DefCurrency(qryFileSelect.Fields[11 + FFirstCol].AsVariant);
  Values.F13 := DefCurrency(qryFileSelect.Fields[12 + FFirstCol].AsVariant);
  Values.F14 := DefCurrency(qryFileSelect.Fields[13 + FFirstCol].AsVariant);
  Values.F15 := DefCurrency(qryFileSelect.Fields[14 + FFirstCol].AsVariant);
  Values.F16 := DefCurrency(qryFileSelect.Fields[15 + FFirstCol].AsVariant);
  Values.F17 := DefCurrency(qryFileSelect.Fields[16 + FFirstCol].AsVariant);
  Values.F18 := DefCurrency(qryFileSelect.Fields[17 + FFirstCol].AsVariant);
  Values.F19 := DefCurrency(qryFileSelect.Fields[18 + FFirstCol].AsVariant);
  Values.F20 := DefCurrency(qryFileSelect.Fields[19 + FFirstCol].AsVariant);
  Values.F21 := DefCurrency(qryFileSelect.Fields[20 + FFirstCol].AsVariant);
  Values.F22 := DefCurrency(qryFileSelect.Fields[21 + FFirstCol].AsVariant);
  Values.F23 := DefCurrency(qryFileSelect.Fields[22 + FFirstCol].AsVariant);
  Values.F24 := DefCurrency(qryFileSelect.Fields[23 + FFirstCol].AsVariant);
  Values.F25 := DefCurrency(qryFileSelect.Fields[24 + FFirstCol].AsVariant);
  Values.F26 := DefCurrency(qryFileSelect.Fields[25 + FFirstCol].AsVariant);
  Values.F27 := DefCurrency(qryFileSelect.Fields[26 + FFirstCol].AsVariant);
  Values.F28 := DefCurrency(qryFileSelect.Fields[27 + FFirstCol].AsVariant);
  Values.F29 := DefCurrency(qryFileSelect.Fields[28 + FFirstCol].AsVariant);
  Values.F30 := Trim(qryFileSelect.Fields[29 + FFirstCol].AsString);
  Values.F31 := DefCurrency(qryFileSelect.Fields[30 + FFirstCol].AsVariant);
  Values.F32 := DefCurrency(qryFileSelect.Fields[31 + FFirstCol].AsVariant);
  Values.F33 := DefCurrency(qryFileSelect.Fields[32 + FFirstCol].AsVariant);
  Values.F34 := DefCurrency(qryFileSelect.Fields[33 + FFirstCol].AsVariant);
  Values.F35 := DefCurrency(qryFileSelect.Fields[34 + FFirstCol].AsVariant);
  Values.F36 := DefCurrency(qryFileSelect.Fields[35 + FFirstCol].AsVariant);
  Values.F37 := DefCurrency(qryFileSelect.Fields[36 + FFirstCol].AsVariant);
  Values.F38 := DefCurrency(qryFileSelect.Fields[37 + FFirstCol].AsVariant);
  Values.F39 := DefCurrency(qryFileSelect.Fields[38 + FFirstCol].AsVariant);
  Values.F40 := DefCurrency(qryFileSelect.Fields[39 + FFirstCol].AsVariant);
  Values.F41 := DefCurrency(qryFileSelect.Fields[40 + FFirstCol].AsVariant);
  Values.F42 := DefCurrency(qryFileSelect.Fields[41 + FFirstCol].AsVariant);
  Values.F43 := DefCurrency(qryFileSelect.Fields[42 + FFirstCol].AsVariant);
  Values.F44 := DefCurrency(qryFileSelect.Fields[43 + FFirstCol].AsVariant);
  Values.F45 := DefCurrency(qryFileSelect.Fields[44 + FFirstCol].AsVariant);
  Values.F46 := DefCurrency(qryFileSelect.Fields[45 + FFirstCol].AsVariant);
  Values.F47 := DefCurrency(qryFileSelect.Fields[46 + FFirstCol].AsVariant);
  Values.F48 := DefCurrency(qryFileSelect.Fields[47 + FFirstCol].AsVariant);
  Values.F49 := DefCurrency(qryFileSelect.Fields[48 + FFirstCol].AsVariant);
  Values.F50 := Trim(qryFileSelect.Fields[49 + FFirstCol].AsString);
  Values.F51 := DefCurrency(qryFileSelect.Fields[50 + FFirstCol].AsVariant);
  Values.F52 := DefCurrency(qryFileSelect.Fields[51 + FFirstCol].AsVariant);
  Values.F53 := DefCurrency(qryFileSelect.Fields[52 + FFirstCol].AsVariant);
  Values.F54 := DefCurrency(qryFileSelect.Fields[53 + FFirstCol].AsVariant);
  Values.F55 := DefCurrency(qryFileSelect.Fields[54 + FFirstCol].AsVariant);
  Values.F56 := DefCurrency(qryFileSelect.Fields[55 + FFirstCol].AsVariant);
  Values.F57 := Trim(qryFileSelect.Fields[56 + FFirstCol].AsString);
  Values.F58 := Trim(qryFileSelect.Fields[57 + FFirstCol].AsString);
  Values.F59 := DefCurrency(qryFileSelect.Fields[58 + FFirstCol].AsVariant);
  Values.F60 := DefCurrency(qryFileSelect.Fields[59 + FFirstCol].AsVariant);
  Values.F61 := DefCurrency(qryFileSelect.Fields[60 + FFirstCol].AsVariant);
  Values.F62 := DefCurrency(qryFileSelect.Fields[61 + FFirstCol].AsVariant);
  Values.F63 := DefCurrency(qryFileSelect.Fields[62 + FFirstCol].AsVariant);
  Values.F64 := DefCurrency(qryFileSelect.Fields[63 + FFirstCol].AsVariant);
  Values.F65 := DefCurrency(qryFileSelect.Fields[64 + FFirstCol].AsVariant);
  Values.F66 := DefCurrency(qryFileSelect.Fields[65 + FFirstCol].AsVariant);
  Values.F67 := DefCurrency(qryFileSelect.Fields[66 + FFirstCol].AsVariant);
  Values.F68 := DefCurrency(qryFileSelect.Fields[67 + FFirstCol].AsVariant);
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
      FContinueOnError := ReadBool(S_INI_DATA, S_CONTINUE_ON_ERROR,
        FContinueOnError);
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

procedure TdmData.Rollback;
begin
  connDB.RollbackTrans();
end;
    
//---------------------------------------------------------------------------

procedure TdmData.SearchForDuplicates;
var
  CurRec: Integer;
  UniqueKey: TUniqueKey;

begin
  CurRec := FFirstRow;
  SetPosition(qryFileSelect, CurRec);

  while CurRec < FLastRow do
  begin
    UniqueKey.RecNo := qryFileSelect.RecNo;
    UniqueKey.Forestry :=
      Trim(qryFileSelect.Fields[0 + FFirstCol].AsString);
    UniqueKey.LocalForestry :=
      Trim(qryFileSelect.Fields[1 + FFirstCol].AsString);
    UniqueKey.Quarter :=
      DefInteger(qryFileSelect.Fields[2 + FFirstCol].AsVariant);
    UniqueKey.Location :=
      DefInteger(qryFileSelect.Fields[3 + FFirstCol].AsVariant);
    UniqueKey.Landuse :=
      Trim(qryFileSelect.Fields[4 + FFirstCol].AsString);
    UniqueKey.LocationArea :=
      DefCurrency(qryFileSelect.Fields[11 + FFirstCol].AsVariant);

    qryFileSelect.Next();

    while qryFileSelect.RecNo <= FLastRow do
    begin
      if (UniqueKey.Forestry =
        Trim(qryFileSelect.Fields[0 + FFirstCol].AsString)) and
        (UniqueKey.LocalForestry =
        Trim(qryFileSelect.Fields[1 + FFirstCol].AsString)) and
        (UniqueKey.Quarter =
        DefInteger(qryFileSelect.Fields[2 + FFirstCol].AsVariant)) and
        (UniqueKey.Location =
        DefInteger(qryFileSelect.Fields[3 + FFirstCol].AsVariant)) and
        (UniqueKey.Landuse =
        Trim(qryFileSelect.Fields[4 + FFirstCol].AsString)) and
        (UniqueKey.LocationArea =
        DefCurrency(qryFileSelect.Fields[11 + FFirstCol].AsVariant)) then
      begin
        ValidationLog(Format(S_LOG_DUPLICATE_ROW, [UniqueKey.RecNo,
          qryFileSelect.RecNo]));
      end;
      qryFileSelect.Next();
    end;

    Inc(CurRec);
    SetPosition(qryFileSelect, CurRec);
  end;
end;

//---------------------------------------------------------------------------

procedure TdmData.SetContinueOnError(Value: Boolean);
begin
  if Value <> FContinueOnError then
    FContinueOnError := Value;
end;

//---------------------------------------------------------------------------

function TdmData.SetPosition(const DataSet: TDataSet;
  const Position: Integer): Boolean;
begin
  DataSet.First();
  Result := DataSet.MoveBy(Position - 1) = (Position - 1);
end;

//---------------------------------------------------------------------------

procedure TdmData.SetValidList(Dictionary: AnsiString; Value: TStringList);
begin
  vld.SetValidList(Dictionary, Value);
end;

//---------------------------------------------------------------------------

function TdmData.StringValidateFile(const RegionID, ForestryID, ReportQuarter,
  ReportYear: Integer): TValidationResult;
var
  RecStatus: string;
  Values: TValuesRec;
  CurRec: Integer;

begin
  FInProgress := True;
  try
    Result := [];

    FScript.Clear();
    FScript.SetScriptHeader();
    vld.InitCheck();

    if ExistsSameReport(ForestryID, ReportQuarter, ReportYear) then
      FScript.AddDelete(RegionID, ForestryID, ReportQuarter, ReportYear);

    for CurRec := FFirstRow to FLastRow do
    begin
      Application.ProcessMessages();
      SetPosition(qryFileSelect, CurRec);
      Progress(qryFileSelect.RecNo);

      if EmptyRec() then
      begin
        ValidationLog(S_LOG_EMPTY_ROW + IntToStr(qryFileSelect.RecNo));
        Continue;
      end;

      ReadDBString(Values);
      Result := Result + vld.StringValidateRecord(qryFileSelect.RecNo, Values,
        RecStatus, ReportYear);

      if vrStop in Result then
      begin
        ValidationLog(S_LOG_FORCE_STOP);
        Break;
      end;

      if (vrDuplicateInvalid in Result) or (vrMainInvalid in Result) or
        (vrExtraInvalid in Result) or (vrStringInvalid in Result)  or
        (vrRelationInvalid in Result) then
          ValidationLog(RecStatus);

      if ((vrMainInvalid in Result) or (vrExtraInvalid in Result) or
        (vrStringInvalid in Result) or (vrRelationInvalid in Result))
        and not FContinueOnError then
          Break;

      FScript.AddInsert(Values, RegionID, ForestryID, ReportQuarter, ReportYear);
    end;

    FScript.SetScriptFooter();
    ValidationLog(#13#10 + DateToStr(Date()) + ' ' + TimeToStr(Time()) + ' ' +
      S_LOG_COMPLETED);

  finally
    FInProgress := False;
  end;
end;

//---------------------------------------------------------------------------

procedure TdmData.ValidationLog(Msg: AnsiString);
begin
  if Assigned(FOnValidationLog) then
    FOnValidationLog(Msg);
end;
 
//---------------------------------------------------------------------------

procedure TdmData.WriteSettings;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      WriteBool(S_INI_DATA, S_CONTINUE_ON_ERROR, FContinueOnError);
      WriteString(S_INI_DATA, S_DB_DATABASE, PgDatabase);
      WriteString(S_INI_DATA, S_DB_SERVER, PgServer);
      WriteString(S_INI_DATA, S_DB_PORT, PgPort);
      WriteString(S_INI_DATA, S_DB_UID, PgUID);
      WriteString(S_INI_DATA, S_DB_PASSWORD, PgPassword);
    finally
      Free();
    end;
end;

//---------------------------------------------------------------------------

end.

