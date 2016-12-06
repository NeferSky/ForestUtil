unit SQLQuery;

interface

type
  TSQLQueries = class(TObject)
  private
    FReadingIndex: Integer;
    FWritingIndex: Integer;
    FActualCount: Integer;
    FModified: Boolean;
    FSQLQueriesArray: array of string;
    procedure ReadQueries;
    procedure SaveQueries;
    procedure ReadSettings;
    procedure SaveSettings;
    function ExistsTheSame(const Value: AnsiString): Boolean;
    function GetBeginOfList: Boolean;
    function GetEndOfList: Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function NextQuery: AnsiString;
    function PrevQuery: AnsiString;
    procedure Add(const Value: AnsiString);
    property BeginOfList: Boolean read GetBeginOfList;
    property EndOfList: Boolean read GetEndOfList;
    property Count: Integer read FActualCount;
    property ItemIndex: Integer read FReadingIndex;
  end;

implementation

uses
  Forms, SysUtils, ForestConsts, ForestTypes, IniFiles;

//---------------------------------------------------------------------------
{ TSQLQueries }

procedure TSQLQueries.Add(const Value: AnsiString);
begin
  if Trim(Value) = '' then
    Exit;
  if ExistsTheSame(Value) then
    Exit;

  if FActualCount < MaxQueriesCount then
  begin
    Inc(FActualCount);
    SetLength(FSQLQueriesArray, FActualCount);
  end;

  if FWritingIndex < FActualCount - 1 then
    Inc(FWritingIndex)
  else
    FWritingIndex := 0;

  try
    FSQLQueriesArray[FWritingIndex] := Value;
    FModified := True;
  except
    raise Exception.Create(Format(E_WRITE_QUERY, [MaxQueriesCount, FActualCount,
      FWritingIndex, FReadingIndex]));
  end;
end;

//---------------------------------------------------------------------------

constructor TSQLQueries.Create(AOwner: TObject);
begin
  FModified := False;
  ReadSettings();
  ReadQueries();
end;

//---------------------------------------------------------------------------

destructor TSQLQueries.Destroy;
begin
  if FModified then
    SaveQueries();
  SaveSettings();

  inherited;
end;

//---------------------------------------------------------------------------

function TSQLQueries.ExistsTheSame(const Value: AnsiString): Boolean;
var
  I: Integer;
  S: AnsiString;

begin
  Result := False;

  S := UpperCase(Value);
  for I := 0 to Length(FSQLQueriesArray) - 1 do
  begin
    Result := S = UpperCase(FSQLQueriesArray[I]);
    if Result then
      Break;
  end;
end;

//---------------------------------------------------------------------------

function TSQLQueries.GetBeginOfList: Boolean;
begin
  Result := FReadingIndex = 0;
end;

//---------------------------------------------------------------------------

function TSQLQueries.GetEndOfList: Boolean;
begin
  Result := (FReadingIndex = FActualCount - 1) or (FActualCount = 0);
end;

//---------------------------------------------------------------------------

function TSQLQueries.NextQuery: AnsiString;
begin
  if not EndOfList then
    try
      Result := FSQLQueriesArray[FReadingIndex + 1];
      Inc(FReadingIndex);
    except
      raise Exception.Create(Format(E_READ_QUERY, [MaxQueriesCount,
        FActualCount, FWritingIndex, FReadingIndex]));
    end;
end;

//---------------------------------------------------------------------------

function TSQLQueries.PrevQuery: AnsiString;
begin
  if not BeginOfList then
    try
      Result := FSQLQueriesArray[FReadingIndex - 1];
      Dec(FReadingIndex);
    except
      raise Exception.Create(Format(E_READ_QUERY, [MaxQueriesCount,
        FActualCount, FWritingIndex, FReadingIndex]));
    end;
end;

//---------------------------------------------------------------------------

procedure TSQLQueries.ReadQueries;
var
  QueryFile: file of TSQLQuery;
  Query: TSQLQuery;

begin
  if not FileExists(S_QUERY_FILE_NAME) then
    Exit;

  Assign(QueryFile, S_QUERY_FILE_NAME);
  Reset(QueryFile);

  try
    while not Eof(QueryFile) do
    begin
      Read(QueryFile, Query);
      Add(Query.QueryText);
    end;
  finally
    CloseFile(QueryFile);
  end;
end;

//---------------------------------------------------------------------------

procedure TSQLQueries.ReadSettings;
begin
  if not FileExists(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME)
    then
    Exit;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      FReadingIndex := ReadInteger(S_INI_QUERIES, S_READING_INDEX, 0);
      FWritingIndex := ReadInteger(S_INI_QUERIES, S_WRITING_INDEX, 0);
      MaxQueriesCount := ReadInteger(S_INI_QUERIES, S_MAX_QUERIES_COUNT, 50);
    finally
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TSQLQueries.SaveQueries;
var
  QueryFile: file of TSQLQuery;
  Query: TSQLQuery;
  I: Integer;

begin
  Assign(QueryFile, S_QUERY_FILE_NAME);
  Rewrite(QueryFile);

  try
    for I := 0 to (Length(FSQLQueriesArray) - 1) do
    begin
      Query.QueryIndex := I;
      Query.QueryText := FSQLQueriesArray[I];
      Write(QueryFile, Query);
    end;
  finally
    CloseFile(QueryFile);
  end;
end;

//---------------------------------------------------------------------------

procedure TSQLQueries.SaveSettings;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) +
    S_SETTINGS_FILE_NAME) do
    try
      WriteInteger(S_INI_QUERIES, S_READING_INDEX, FReadingIndex);
      WriteInteger(S_INI_QUERIES, S_WRITING_INDEX, FWritingIndex);
      WriteInteger(S_INI_QUERIES, S_MAX_QUERIES_COUNT, MaxQueriesCount);
    finally
      Free();
    end;
end;

end.

