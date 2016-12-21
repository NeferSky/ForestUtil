unit Dictionary;

interface

uses
  Windows, Classes, Forms, SysUtils, ForestConsts, ForestTypes, Dialogs;

type
  TDictionary = class(TObject)
  private
    FDictionaryName: AnsiString;
    FDictArray: array of TDictRecord;
    FValidArray: TValidArr;
    FForceSkip: Boolean;
    function Equal(const A: TDictRecord; const B: TDictRecord): Boolean;
    function GetValidArray: TValidArr;
    procedure SetValidArray(Value: TValidArr);
    function GetForceSkip: Boolean;
    procedure SetForceSkip(Value: Boolean);
  public
    function Validate(const AWord: AnsiString): Integer;
    function FindRecord(const AWord: AnsiString;
      var WordRecord: TDictRecord): Boolean;
    procedure WriteRecord(const WordRecord: TDictRecord);
    procedure Clear;
    constructor Create(AFile: AnsiString);
    destructor Destroy; override;
    property ValidList: TValidArr read GetValidArray write SetValidArray;
    property ForceSkip: Boolean read GetForceSkip write SetForceSkip;
  end;

var
  DictionaryFile: file of TDictRecord;
  ValidFile: file of TValidRecord;

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

  ValidDictFile := Format('%s%s%s', [GetAppPath, S_DICTIONARY_VALID_PREFIX,
    FDictionaryName]);
  if FileExists(ValidDictFile) then
  begin
    Assign(ValidFile, ValidDictFile);
    Reset(ValidFile);
    while not Eof(ValidFile) do
    begin
      I := Length(FValidArray);
      SetLength(FValidArray, I + 1);
      Read(ValidFile, FValidArray[I]);
    end;
    CloseFile(ValidFile);
  end;
end;

//---------------------------------------------------------------------------

destructor TDictionary.Destroy;
var
  DictFile, ValidDictFile: AnsiString;
  I: Integer;
  ValidRecord: TValidRecord;

begin
  DictFile := Format('%s%s', [GetAppPath, FDictionaryName]);
  Assign(DictionaryFile, DictFile);
  Rewrite(DictionaryFile);
  for I := 0 to (Length(FDictArray) - 1) do
    Write(DictionaryFile, FDictArray[I]);
  CloseFile(DictionaryFile);

  Clear();

  ValidDictFile := Format('%s%s%s', [GetAppPath, S_DICTIONARY_VALID_PREFIX,
    FDictionaryName]);
  Assign(ValidFile, ValidDictFile);
  Rewrite(ValidFile);
  for I := 0 to (Length(FValidArray) - 1) do
    Write(ValidFile, FValidArray[I]);
  CloseFile(ValidFile);

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

function TDictionary.GetForceSkip: Boolean;
begin
  Result := FForceSkip;
end;
 
//---------------------------------------------------------------------------

function TDictionary.GetValidArray: TValidArr;
begin
  Result := FValidArray;
end;

//---------------------------------------------------------------------------

procedure TDictionary.SetForceSkip(Value: Boolean);
begin
  if Value <> FForceSkip then
    FForceSkip := Value;
end;
   
//---------------------------------------------------------------------------

procedure TDictionary.SetValidArray(Value: TValidArr);
begin
  FValidArray := Value;
end;

//---------------------------------------------------------------------------

function TDictionary.Validate(const AWord: AnsiString): Integer;
var
  I: Integer;

begin
  Result := 0;

  for I := 0 to Length(FValidArray) - 1 do
    if FValidArray[I].WordValue = Uppercase(AWord) then
    begin
      Result := FValidArray[I].WordIndex;
      Break;
    end;
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

