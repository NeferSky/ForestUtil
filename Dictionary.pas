unit Dictionary;

interface

uses
  Windows, Classes, Forms, SysUtils, ForestConsts, ForestTypes, Dialogs;

type
  TDictionary = class(TObject)
  private
    FDictionaryName: AnsiString;
    FDictArray: array of TDictRecord;
    FValidList: TStringList;
    function Equal(const A: TDictRecord; const B: TDictRecord): Boolean;
    function GetValidList: TStringList;
    procedure SetValidList(Value: TStringList);
  public
    function Validate(const AWord: AnsiString): Boolean;
    function FindRecord(const AWord: AnsiString; var WordRecord: TDictRecord):
      Boolean;
    procedure WriteRecord(const WordRecord: TDictRecord);
    procedure Clear;
    constructor Create(AFile: AnsiString);
    destructor Destroy; override;
    property ValidList: TStringList read GetValidList write SetValidList;
  end;

var
  DictionaryFile: file of TDictRecord;

implementation

uses
  NsUtils;

//---------------------------------------------------------------------------
{ TDictionary }

procedure TDictionary.Clear;
begin
  SetLength(FDictArray, 0);
end;

//---------------------------------------------------------------------------

constructor TDictionary.Create(AFile: AnsiString);
var
  DictFile, ValidDictFile: AnsiString;
  I: Integer;

begin
  FDictionaryName := AFile;

  Clear();

  DictFile := Format('%s%s', [GetAppPath, FDictionaryName]);
  if FileExists(DictFile) then
  begin
    Assign(DictionaryFile, DictFile);
    Reset(DictionaryFile);
    while not Eof(DictionaryFile) do
    begin
      I := Length(FDictArray);
      SetLength(FDictArray, I + 1);
      Read(DictionaryFile, FDictArray[I]);
    end;
    CloseFile(DictionaryFile);
  end;

  FValidList := TStringList.Create();
  ValidDictFile := Format('%s%s%s', [GetAppPath,
    S_DICTIONARY_VALID_PREFIX, FDictionaryName]);
  if FileExists(ValidDictFile) then
    FValidList.LoadFromFile(ValidDictFile);
end;

//---------------------------------------------------------------------------

destructor TDictionary.Destroy;
var
  DictFile, ValidDictFile: AnsiString;
  I: Integer;

begin
  DictFile := Format('%s%s', [GetAppPath, FDictionaryName]);
  Assign(DictionaryFile, DictFile);
  Rewrite(DictionaryFile);
  for I := 0 to (Length(FDictArray) - 1) do
    Write(DictionaryFile, FDictArray[I]);
  CloseFile(DictionaryFile);

  Clear();

  ValidDictFile := Format('%s%s%s', [GetAppPath,
    S_DICTIONARY_VALID_PREFIX, FDictionaryName]);
  FValidList.SaveToFile(ValidDictFile);
  FValidList.Clear();
  FValidList.Free();

  inherited;
end;

//---------------------------------------------------------------------------

function TDictionary.Equal(const A: TDictRecord; const B: TDictRecord): Boolean;
begin
  Result := (A.OldWord = B.OldWord);
end;

//---------------------------------------------------------------------------

function TDictionary.FindRecord(const AWord: AnsiString;
  var WordRecord: TDictRecord): Boolean;
var
  I: Integer;

begin
  Result := False;

  for I := 0 to (Length(FDictArray) - 1) do
    if FDictArray[I].OldWord = AWord then
    begin
      WordRecord := FDictArray[I];
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------

function TDictionary.GetValidList: TStringList;
begin
  Result := FValidList;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetValidList(Value: TStringList);
begin
  FValidList.Assign(Value);
end;

//---------------------------------------------------------------------------

function TDictionary.Validate(const AWord: AnsiString): Boolean;
begin
  Result := FValidList.IndexOf(UpperCase(Trim(AWord))) <> -1;
end;

//---------------------------------------------------------------------------

procedure TDictionary.WriteRecord(const WordRecord: TDictRecord);
var
  I: Integer;

begin
  for I := 0 to (Length(FDictArray) - 1) do
    if Equal(FDictArray[I], WordRecord) then
    begin
      FDictArray[I].NewWord := UpperCase(WordRecord.NewWord);
      Exit;
    end;

  I := Length(FDictArray);
  SetLength(FDictArray, I + 1);
  FDictArray[I] := WordRecord;
end;

end.

