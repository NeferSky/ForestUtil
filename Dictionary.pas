unit Dictionary;

interface

uses
  Windows, Classes, Forms, SysUtils, ForestConsts, ForestTypes, Dialogs;

type
  TDictionary = class(TObject)
  private
    FDictionaryFile: AnsiString;
    FCaption: AnsiString;
    FCatalogArray: TCatalogArr;
    FDictArray: TDictArr;
    FForceSkip: Boolean;
    FDictionaryFormatString: AnsiString;
    FSQLScript: AnsiString;
    FDictionaryList: TStringList;
    procedure ReadSettings;
    procedure WriteSettings;
    function Equal(const A: TCatalogRecord; const B: TCatalogRecord): Boolean;
    procedure PrepareValuesList;
    //
    function GetCaption: AnsiString;
    procedure SetCaption(Value: AnsiString);
    function GetDictionaryArray: TDictArr;
    procedure SetDictionaryArray(Value: TDictArr);
    function GetForceSkip: Boolean;
    procedure SetForceSkip(Value: Boolean);
    function GetDictionaryFormatString: AnsiString;
    procedure SetDictionaryFormatString(Value: AnsiString);
    function GetSQLScript: AnsiString;
    procedure SetSQLScript(Value: AnsiString);
    function GetCatalogFile: AnsiString;
    function GetCatalogArray: TCatalogArr;
    procedure SetCatalogArray(Value: TCatalogArr);
  public
    function Validate(const AWord: AnsiString;
      const RelationID: Integer): Integer;
    function FindRecord(const AWord: AnsiString; const RelationID:
      Integer; var WordRecord: TCatalogRecord): Boolean;
    function GetRecordByListItem(const S: AnsiString): TDictRecord;
    procedure WriteRecord(const WordRecord: TCatalogRecord);
    procedure Clear;
    //
    constructor Create(AFile: AnsiString; DictCaption: AnsiString);
    destructor Destroy; override;
    //--
    property Caption: AnsiString read GetCaption write SetCaption;
    property DictionaryArr: TDictArr read GetDictionaryArray
      write SetDictionaryArray;
    property CatalogArr: TCatalogArr read GetCatalogArray write SetCatalogArray;
    property DictionaryList: TStringList read FDictionaryList;
    property ForceSkip: Boolean read GetForceSkip write SetForceSkip;
    property DictionaryFormatString: AnsiString read GetDictionaryFormatString
      write SetDictionaryFormatString;
    property DictionaryFile: AnsiString read FDictionaryFile;
    property CatalogFile: AnsiString read GetCatalogFile;
    property SQLScript: AnsiString read GetSQLScript write SetSQLScript;
  end;

var
  DictionaryFile: file of TDictRecord;
  CatalogFile: file of TCatalogRecord;

implementation

uses
  IniFiles, NsUtils;

//---------------------------------------------------------------------------
{ TDictionary }

procedure TDictionary.Clear;
begin
  SetLength(FDictArray, 0);
end;

//---------------------------------------------------------------------------

constructor TDictionary.Create(AFile: AnsiString; DictCaption: AnsiString);
var
  DictFilePath, CatalogFilePath: AnsiString;
  DictFile: File of TDictRecord;
  CatalogFile: File of TCatalogRecord;
  I: Integer;

begin
  FDictionaryFile := AFile;
  FCaption := DictCaption;
  FDictionaryList := TStringList.Create();

  ReadSettings();
  Clear();

  CatalogFilePath := Format('%s%s', [GetAppPath, FDictionaryFile]);
  if FileExists(CatalogFilePath) then
  begin
    Assign(CatalogFile, CatalogFilePath);
    Reset(CatalogFile);
    while not Eof(CatalogFile) do
    begin
      I := Length(FCatalogArray);
      SetLength(FCatalogArray, I + 1);
      Read(CatalogFile, FCatalogArray[I]);
    end;
    CloseFile(CatalogFile);
  end;

  DictFilePath := Format('%s%s%s', [GetAppPath, S_DICT_VALID_PREFIX, FDictionaryFile]);
  if FileExists(DictFilePath) then
  begin
    Assign(DictFile, DictFilePath);
    Reset(DictFile);
    while not Eof(DictFile) do
    begin
      I := Length(FDictArray);
      SetLength(FDictArray, I + 1);
      Read(DictFile, FDictArray[I]);
    end;
    CloseFile(DictFile);
  end;

  PrepareValuesList();
end;

//---------------------------------------------------------------------------

destructor TDictionary.Destroy;
var
  CatalogFilePath, DictFilePath: AnsiString;
  CatalogFile: File of TCatalogRecord;
  DictFile: File of TDictRecord;
  I: Integer;

begin
  CatalogFilePath := Format('%s%s', [GetAppPath, FDictionaryFile]);
  Assign(CatalogFile, CatalogFilePath);
  Rewrite(CatalogFile);
  for I := 0 to (Length(FCatalogArray) - 1) do
    Write(CatalogFile, FCatalogArray[I]);
  CloseFile(CatalogFile);

  DictFilePath := Format('%s%s%s', [GetAppPath, S_DICT_VALID_PREFIX, FDictionaryFile]);
  Assign(DictFile, DictFilePath);
  Rewrite(DictFile);
  for I := 0 to (Length(FDictArray) - 1) do
    Write(DictFile, FDictArray[I]);
  CloseFile(DictFile);

  Clear();
  FDictionaryList.Clear();
  FDictionaryList.Destroy;

  WriteSettings();

  inherited;
end;

//---------------------------------------------------------------------------

function TDictionary.Equal(const A: TCatalogRecord; const B: TCatalogRecord): Boolean;
begin
  Result := (A.OldWord = B.OldWord);
end;

//---------------------------------------------------------------------------

function TDictionary.FindRecord(const AWord: AnsiString; const RelationID:
  Integer; var WordRecord: TCatalogRecord): Boolean;
var
  I: Integer;

begin
  Result := False;

  for I := 0 to (Length(FCatalogArray) - 1) do
    if (FCatalogArray[I].OldWord = AWord) and
      (FCatalogArray[I].RelationID = RelationID) then
    begin
      WordRecord := FCatalogArray[I];
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------

function TDictionary.GetCaption: AnsiString;
begin
  Result := FCaption;
end;

//---------------------------------------------------------------------------
  
function TDictionary.GetCatalogArray: TCatalogArr;
begin
  Result := FCatalogArray;
end;

//---------------------------------------------------------------------------

function TDictionary.GetCatalogFile: AnsiString;
begin
  Result := S_DICT_VALID_PREFIX + FDictionaryFile;
end;

//---------------------------------------------------------------------------

function TDictionary.GetDictionaryArray: TDictArr;
begin
  Result := FDictArray;
end;

//---------------------------------------------------------------------------

function TDictionary.GetDictionaryFormatString: AnsiString;
begin
  Result := FDictionaryFormatString;
end;
   
//---------------------------------------------------------------------------

function TDictionary.GetForceSkip: Boolean;
begin
  Result := FForceSkip;
end;

//---------------------------------------------------------------------------

function TDictionary.GetRecordByListItem(const S: AnsiString): TDictRecord;
begin
  Result := FDictArray[FDictionaryList.IndexOf(S)];
end;

//---------------------------------------------------------------------------

function TDictionary.GetSQLScript: AnsiString;
begin
  Result := FSQLScript;
end;

//---------------------------------------------------------------------------

procedure TDictionary.PrepareValuesList;
var
  I: Integer;
  TmpStr: AnsiString;

begin
  FDictionaryList.Clear();

  for I := 0 to Length(FDictArray) - 1 do
  begin
    TmpStr := FDictionaryFormatString;
    TmpStr := StringReplace(TmpStr, S_DICT_NAME_FORMAT, FDictArray[I].WordValue, [rfReplaceAll, rfIgnoreCase]);
    TmpStr := StringReplace(TmpStr, S_DICT_ID_FORMAT, IntToStr(FDictArray[I].WordIndex), [rfReplaceAll, rfIgnoreCase]);
    FDictionaryList.Add(TmpStr);
  end;
end;

//---------------------------------------------------------------------------

procedure TDictionary.ReadSettings;
begin
  if not FileExists(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME) then
    Exit;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME) do
    try
      FDictionaryFormatString := ReadString(S_INI_DICT_FORMATS, FDictionaryFile, S_DICT_NAME_FORMAT);
    finally
      Free();
    end;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetCaption(Value: AnsiString);
begin
  if FCaption <> Value then
    FCaption := Value;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetCatalogArray(Value: TCatalogArr);
begin
  if Value <> FCatalogArray then
    FCatalogArray := Value;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetForceSkip(Value: Boolean);
begin
  if Value <> FForceSkip then
    FForceSkip := Value;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetSQLScript(Value: AnsiString);
begin
  if FSQLScript <> Value then
    FSQLScript := Value;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetDictionaryArray(Value: TDictArr);
begin
  if Value <> FDictArray then
  begin
    FDictArray := Value;
    PrepareValuesList();
  end;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetDictionaryFormatString(Value: AnsiString);
begin
  if Value <> FDictionaryFormatString then
  begin
    FDictionaryFormatString := Value;
    PrepareValuesList();
  end;
end;

//---------------------------------------------------------------------------

function TDictionary.Validate(const AWord: AnsiString;
  const RelationID: Integer): Integer;
var
  I: Integer;

begin
  Result := -1;

  for I := 0 to Length(FDictArray) - 1 do
    if (FDictArray[I].WordValue = AnsiUpperCase(AWord)) and
       (FDictArray[I].RelationID = RelationID) then
    begin
      Result := FDictArray[I].WordIndex;
      Break;
    end;
end;

//---------------------------------------------------------------------------

procedure TDictionary.WriteRecord(const WordRecord: TCatalogRecord);
var
  I: Integer;

begin
  for I := 0 to (Length(FCatalogArray) - 1) do
    if Equal(FCatalogArray[I], WordRecord) then
    begin
      FCatalogArray[I].NewWord := AnsiUpperCase(WordRecord.NewWord);
      Exit;
    end;

  I := Length(FCatalogArray);
  SetLength(FCatalogArray, I + 1);
  FCatalogArray[I] := WordRecord;
end;
    
//---------------------------------------------------------------------------

procedure TDictionary.WriteSettings;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + S_SETTINGS_FILE_NAME) do
    try
      WriteString(S_INI_DICT_FORMATS, FDictionaryFile, FDictionaryFormatString);
    finally
      Free();
    end;
end;

end.

