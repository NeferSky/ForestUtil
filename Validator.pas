unit Validator;

interface

uses
  Dictionary, Classes, ForestTypes;

type
  TValidator = class(TObject)
  private
    DictForestries: TDictionary;
    DictLocalForestries: TDictionary;
    DictLanduse: TDictionary;
    DictProtectCategory: TDictionary;
    DictSpecies: TDictionary;
    DictDamage: TDictionary;
    DictPest: TDictionary;

    FRecNo: Integer;
    FRecordStatus: AnsiString;

    function MainValidateRecord(var CurrentRecord: TValuesRec): TValidationResult;
    function ExtraValidateRecord(var CurrentRecord: TValuesRec): TValidationResult;
    function StringValidateField(var Field: AnsiString; var FieldID: Integer;
      const Dict: TDictionary; const Prompt: AnsiString): TValidationResult;
    function RelationValidateRecord(var CurrentRecord: TValuesRec;
      const ReportYear: Integer): TValidationResult;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    procedure InitCheck;
    function GetValidList(Dictionary: AnsiString): TValidArr;
    procedure SetValidList(Dictionary: AnsiString; Value: TValidArr);

    function MathValidateRecord(const RecNo: Integer;
      var CurrentRecord: TValuesRec): TValidationResult;
    function StringValidateRecord(const RecNo: Integer;
      var CurrentRecord: TValuesRec; const ReportYear: Integer): TValidationResult;

    property RecordStatus: AnsiString read FRecordStatus;
  end;

implementation

uses
  SysUtils, Edit, ForestConsts, Data;

//---------------------------------------------------------------------------
{ TValidator }

constructor TValidator.Create(Owner: TObject);
begin
  DictForestries := TDictionary.Create(S_DICTIONARY_FORESTRIES_FILE);
  DictLocalForestries := TDictionary.Create(S_DICTIONARY_LOCAL_FORESTRIES_FILE);
  DictLanduse := TDictionary.Create(S_DICTIONARY_LANDUSE_FILE);
  DictProtectCategory := TDictionary.Create(S_DICTIONARY_PROTECT_CATEGORY_FILE);
  DictSpecies := TDictionary.Create(S_DICTIONARY_SPECIES_FILE);
  DictDamage := TDictionary.Create(S_DICTIONARY_DAMAGE_FILE);
  DictPest := TDictionary.Create(S_DICTIONARY_PEST_FILE);
end;

//---------------------------------------------------------------------------

destructor TValidator.Destroy;
begin
  DictForestries.Free();
  DictLocalForestries.Free();
  DictLanduse.Free();
  DictProtectCategory.Free();
  DictSpecies.Free();
  DictDamage.Free();
  DictPest.Free();

  inherited;
end;

//---------------------------------------------------------------------------

function TValidator.ExtraValidateRecord(var CurrentRecord: TValuesRec): TValidationResult;
begin
  if CurrentRecord.F59 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 59, 13]);
  end;
  if CurrentRecord.F60 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 60, 13]);
  end;
  if CurrentRecord.F61 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 61, 13]);
  end;
  if CurrentRecord.F62 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 62, 13]);
  end;
  if CurrentRecord.F63 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 63, 13]);
  end;
  if CurrentRecord.F64 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 64, 13]);
  end;
  if CurrentRecord.F65 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 65, 13]);
  end;
  if CurrentRecord.F66 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 66, 13]);
  end;
  if CurrentRecord.F67 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 67, 13]);
  end;
  if CurrentRecord.F68 > CurrentRecord.F13 then
  begin
    Result := [vrExtraInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [FRecNo, 68, 13]);
  end;
end;

//---------------------------------------------------------------------------

function TValidator.GetValidList(Dictionary: AnsiString): TValidArr;
begin
  if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_FORESTRIES_FILE) then
    Result := DictForestries.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_LOCAL_FORESTRIES_FILE) then
    Result := DictLocalForestries.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_LANDUSE_FILE) then
    Result := DictLanduse.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_PROTECT_CATEGORY_FILE) then
    Result := DictProtectCategory.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_SPECIES_FILE) then
    Result := DictSpecies.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_DAMAGE_FILE) then
    Result := DictDamage.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX + S_DICTIONARY_PEST_FILE) then
    Result := DictPest.ValidList;
end;

//---------------------------------------------------------------------------

procedure TValidator.InitCheck;
begin
  DictForestries.ForceSkip := False;
  DictLocalForestries.ForceSkip := False;
  DictLanduse.ForceSkip := False;
  DictProtectCategory.ForceSkip := False;
  DictSpecies.ForceSkip := False;
  DictDamage.ForceSkip := False;
  DictPest.ForceSkip := False;
end;

//---------------------------------------------------------------------------

function TValidator.MainValidateRecord(var CurrentRecord: TValuesRec): TValidationResult;
var
  Tmp: Currency;

begin
  Result := [];
  FRecordStatus := '';

  // 2
  if CurrentRecord.F13 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 13, 12]);
  end;

  // 3
  if (CurrentRecord.F14 + CurrentRecord.F15 + CurrentRecord.F16
    <> CurrentRecord.F13) then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [FRecNo, '14-16', 13]);
  end;
  if CurrentRecord.F14 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 14, 13]);
  end;
  if CurrentRecord.F15 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 15, 13]);
  end;
  if CurrentRecord.F16 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 16, 13]);
  end;

  // 4
  if CurrentRecord.F17 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 17, 13]);
  end;
  if CurrentRecord.F18 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 18, 13]);
  end;
  if CurrentRecord.F19 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 19, 13]);
  end;
  if CurrentRecord.F20 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 20, 13]);
  end;
  if CurrentRecord.F21 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 21, 13]);
  end;
  if CurrentRecord.F22 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 22, 13]);
  end;
  if CurrentRecord.F23 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 23, 13]);
  end;
  if CurrentRecord.F24 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 24, 13]);
  end;
  if CurrentRecord.F25 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 25, 13]);
  end;
  if CurrentRecord.F26 > CurrentRecord.F13 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 26, 13]);
  end;

  // 5
  if (CurrentRecord.F17 + CurrentRecord.F18 - CurrentRecord.F34 -
    CurrentRecord.F40 - CurrentRecord.F46 - CurrentRecord.F53) <>
    CurrentRecord.F19 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO,
      [FRecNo, '17+18-34-40-46-53', 19]);
  end;

  // 6
  if (CurrentRecord.F20 + CurrentRecord.F21 + CurrentRecord.F22 +
    CurrentRecord.F23) <> CurrentRecord.F19 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [FRecNo, '20-23', 19]);
  end;

  // 7
  if CurrentRecord.F24 > CurrentRecord.F17 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 24, 17]);
  end;
  if CurrentRecord.F25 > CurrentRecord.F18 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 25, 18]);
  end;
  if CurrentRecord.F26 > CurrentRecord.F19 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 26, 19]);
  end;

  // 8
  Tmp := CurrentRecord.F17 + CurrentRecord.F18;
  if CurrentRecord.F27 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 27, '17-18']);
  end;
  if CurrentRecord.F28 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 28, '17-18']);
  end;
  if CurrentRecord.F29 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 29, '17-18']);
  end;
  {  if CurrentRecord.F30 > Tmp then
    begin
      Result := [vrMainInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
        [FRecNo, 30, '17-18']);
    end;
    }
  if CurrentRecord.F31 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
      [FRecNo, 31, '17-18']);
  end;
  if CurrentRecord.F32 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 32, '17-18']);
  end;
  if CurrentRecord.F33 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 33, '17-18']);
  end;
  if CurrentRecord.F34 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 34, '17-18']);
  end;
  if CurrentRecord.F35 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 35, '17-18']);
  end;
  if CurrentRecord.F38 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 38, '17-18']);
  end;
  if CurrentRecord.F39 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 39, '17-18']);
  end;
  if CurrentRecord.F40 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 40, '17-18']);
  end;
  if CurrentRecord.F41 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 41, '17-18']);
  end;
  if CurrentRecord.F44 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 44, '17-18']);
  end;
  if CurrentRecord.F45 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 45, '17-18']);
  end;
  if CurrentRecord.F46 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 46, '17-18']);
  end;
  if CurrentRecord.F47 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 47, '17-18']);
  end;
  if CurrentRecord.F51 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 51, '17-18']);
  end;
  if CurrentRecord.F52 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 52, '17-18']);
  end;
  if CurrentRecord.F53 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 53, '17-18']);
  end;
  if CurrentRecord.F54 > Tmp then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [FRecNo, 54, '17-18']);
  end;

  //9
  if CurrentRecord.F32 > CurrentRecord.F27 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 32, 27]);
  end;
  if CurrentRecord.F33 > CurrentRecord.F27 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 33, 27]);
  end;
  if CurrentRecord.F34 > CurrentRecord.F27 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 34, 27]);
  end;
  if CurrentRecord.F35 > CurrentRecord.F27 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 35, 27]);
  end;
  if CurrentRecord.F38 > CurrentRecord.F28 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 38, 28]);
  end;
  if CurrentRecord.F39 > CurrentRecord.F28 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 39, 28]);
  end;
  if CurrentRecord.F40 > CurrentRecord.F28 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 40, 28]);
  end;
  if CurrentRecord.F41 > CurrentRecord.F28 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 41, 28]);
  end;
  if CurrentRecord.F44 > CurrentRecord.F29 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 44, 29]);
  end;
  if CurrentRecord.F45 > CurrentRecord.F29 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 45, 29]);
  end;
  if CurrentRecord.F46 > CurrentRecord.F29 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 46, 29]);
  end;
  if CurrentRecord.F47 > CurrentRecord.F29 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 47, 29]);
  end;
  if CurrentRecord.F51 > CurrentRecord.F31 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 51, 31]);
  end;
  if CurrentRecord.F52 > CurrentRecord.F31 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 52, 31]);
  end;
  if CurrentRecord.F53 > CurrentRecord.F31 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 53, 31]);
  end;
  if CurrentRecord.F54 > CurrentRecord.F31 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 54, 31]);
  end;

  //10
  if CurrentRecord.F32 > CurrentRecord.F34 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 32, 34]);
  end;
  if CurrentRecord.F33 > CurrentRecord.F35 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 33, 35]);
  end;

  //11
  if CurrentRecord.F38 > CurrentRecord.F40 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 38, 40]);
  end;
  if CurrentRecord.F39 > CurrentRecord.F41 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 39, 41]);
  end;

  //12
  if CurrentRecord.F44 > CurrentRecord.F46 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 44, 46]);
  end;
  if CurrentRecord.F45 > CurrentRecord.F47 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 45, 47]);
  end;

  //13
  if CurrentRecord.F51 > CurrentRecord.F53 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 51, 53]);
  end;
  if CurrentRecord.F52 > CurrentRecord.F54 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 52, 54]);
  end;

  //14
  if CurrentRecord.F33 > CurrentRecord.F32 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 33, 32]);
  end;
  if CurrentRecord.F35 > CurrentRecord.F34 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 35, 34]);
  end;
  if CurrentRecord.F39 > CurrentRecord.F38 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 39, 38]);
  end;
  if CurrentRecord.F41 > CurrentRecord.F40 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 41, 40]);
  end;
  if CurrentRecord.F45 > CurrentRecord.F44 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 45, 44]);
  end;
  if CurrentRecord.F47 > CurrentRecord.F46 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 47, 46]);
  end;
  if CurrentRecord.F52 > CurrentRecord.F51 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 52, 51]);
  end;
  if CurrentRecord.F54 > CurrentRecord.F53 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 54, 53]);
  end;

  //15
  if (CurrentRecord.F59 + CurrentRecord.F60 - CurrentRecord.F61 -
    CurrentRecord.F62) > CurrentRecord.F63 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO,
      [FRecNo, '59+60-61-62', 63]);
  end;

  //16
  if CurrentRecord.F64 > CurrentRecord.F63 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 64, 63]);
  end;

  // 17
  if (CurrentRecord.F65 + CurrentRecord.F66 + CurrentRecord.F67 +
    CurrentRecord.F68 <> CurrentRecord.F63) then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [FRecNo, '65-68', 63]);
  end;

  //18
  if CurrentRecord.F59 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 59, 12]);
  end;
  if CurrentRecord.F60 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 60, 12]);
  end;
  if CurrentRecord.F61 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 61, 12]);
  end;
  if CurrentRecord.F62 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 62, 12]);
  end;
  if CurrentRecord.F63 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 63, 12]);
  end;
  if CurrentRecord.F64 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 64, 12]);
  end;
  if CurrentRecord.F65 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 65, 12]);
  end;
  if CurrentRecord.F66 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 66, 12]);
  end;
  if CurrentRecord.F67 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 67, 12]);
  end;
  if CurrentRecord.F68 > CurrentRecord.F12 then
  begin
    Result := [vrMainInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [FRecNo, 68, 12]);
  end;
end;
    
//---------------------------------------------------------------------------

function TValidator.MathValidateRecord(const RecNo: Integer;
  var CurrentRecord: TValuesRec): TValidationResult;
begin
  Result := [];
  FRecordStatus := '';
  FRecNo := RecNo;

  Result := Result + MainValidateRecord(CurrentRecord);
  Result := Result + ExtraValidateRecord(CurrentRecord);
end;

//---------------------------------------------------------------------------

function TValidator.RelationValidateRecord(var CurrentRecord: TValuesRec;
  const ReportYear: Integer): TValidationResult;
var
  CauseCode: Integer;

begin
  // Species relation check
  if not dmData.CheckSpeciesRelation(CurrentRecord.F8, CurrentRecord.F9) then
  begin
    Result := Result + [vrRelationInvalid];
    FRecordStatus := FRecordStatus + Format(S_LOG_NO_SPECIES_RELATION, [FRecNo]);
  end;

  // Cause relation check
//  CauseCode := dmData.GetIntField(Format(S_DB_GET_CAUSE_CODE, [CurrentRecord.F9]));

  case CauseCode of
  881, 882, 883, 870, 871, 872, 873, 874, 875:
    if CurrentRecord.F10 <> ReportYear then
    begin
      Result := Result + [vrRelationInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]);
    end;
  821, 822, 823:
    if CurrentRecord.F10 >= ReportYear then
    begin
      Result := Result + [vrRelationInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]);
    end;
  851, 854, 856, 863, 864, 865:
    if (CurrentRecord.F10 > ReportYear - 1) or
    (CurrentRecord.F10 < ReportYear - 3) then
    begin
      Result := Result + [vrRelationInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]);
    end;
  852, 855, 857, 866, 867, 868:
    if (CurrentRecord.F10 > ReportYear - 4) or
    (CurrentRecord.F10 < ReportYear - 10) then
    begin
      Result := Result + [vrRelationInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]);
    end;
  853, 858, 869:
    if (CurrentRecord.F10 > ReportYear - 10) then
    begin
      Result := Result + [vrRelationInvalid];
      FRecordStatus := FRecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]);
    end;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.SetValidList(Dictionary: AnsiString; Value: TValidArr);
begin
  if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_FORESTRIES_FILE) then
    DictForestries.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LOCAL_FORESTRIES_FILE) then
    DictLocalForestries.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LANDUSE_FILE) then
    DictLanduse.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PROTECT_CATEGORY_FILE) then
    DictProtectCategory.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_SPECIES_FILE) then
    DictSpecies.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_DAMAGE_FILE) then
    DictDamage.ValidList := Value

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PEST_FILE) then
    DictPest.ValidList := Value;
end;

//---------------------------------------------------------------------------

function TValidator.StringValidateField(var Field: AnsiString;
  var FieldID: Integer; const Dict: TDictionary;
  const Prompt: AnsiString): TValidationResult;
var
  DictRecord: TDictRecord;
  Magic: AnsiString;
  WordIndex: Integer;

begin
  if Dict.ForceSkip then
    Exit;

  // Empty string - ask to replace and not remember
  if Trim(Field) = '' then
  begin
    case ShowEdit(Field, Prompt, Dict.ValidList) of
    srReplace:
    begin
      Field := frmEdit.cmbSynonim.Text;
      FieldID := frmEdit.CurrentIndex;
    end;
    srForceSkip:
      Dict.ForceSkip := True;
    srStop:
      Result := Result + [vrStop];
    end;
    Exit;
  end;

  WordIndex := Dict.Validate(Field);
  if WordIndex = 0 then
  begin
    // Found in dictionary - autoreplace, log only
    if Dict.FindRecord(Field, DictRecord) then
    begin
      FRecordStatus := FRecordStatus + Format(S_LOG_REPLACE_FROM_DICTIONARY,
        [FRecNo, DictRecord.NewWord, Field]);
    end

    // Not found in dictionary - ask to replace, remember and log
    else
    begin
      // here ask...
      case ShowEdit(Field, Prompt, Dict.ValidList) of
      srReplace:
      begin
        // ...here remember...
        DictRecord.OldWord := Field;
        DictRecord.NewWord := frmEdit.cmbSynonim.Text;
        DictRecord.NewIndex := frmEdit.CurrentIndex;
        Dict.WriteRecord(DictRecord);
        // ...here log...
        FRecordStatus := FRecordStatus + Format(S_LOG_REPLACE_FROM_DICTIONARY,
          [FRecNo, DictRecord.NewWord, Field]);
        // ...here replace
        Field := frmEdit.cmbSynonim.Text;
        FieldID := frmEdit.CurrentIndex;
      end;
      srStop:
      begin
        Result := Result + [vrStop];
        Exit;
      end;
      srSkip:
        Exit;
      srForceSkip:
        Dict.ForceSkip := True;
      end;
    end;

    // Some magic to set new word into variable
    FieldID := DictRecord.NewIndex;
    Magic := DictRecord.NewWord;
    Field := Magic;
  end
  else
    FieldID := WordIndex;
end;

//---------------------------------------------------------------------------

function TValidator.StringValidateRecord(const RecNo: Integer;
  var CurrentRecord: TValuesRec; const ReportYear: Integer): TValidationResult;
var
  Prompt: AnsiString;

begin
  Result := [];
  FRecordStatus := '';
  FRecNo := RecNo;
{
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[2], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[1], CurrentRecord.F1]);
  Result := Result + StringValidateField(CurrentRecord.F2, CurrentRecord.I2,
    DictLocalForestries, Prompt);
  if vrStop in Result then
    Exit;
 }
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[5], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[6], CurrentRecord.F6]);
  Result := Result + StringValidateField(CurrentRecord.F5, CurrentRecord.I5,
    DictLanduse, Prompt);
  if vrStop in Result then
    Exit;

  Result := Result + StringValidateField(CurrentRecord.F6, CurrentRecord.I6,
    DictProtectCategory, ARR_FIELD_NAMES[6]);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[7], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[8], CurrentRecord.F8]);
  Result := Result + StringValidateField(CurrentRecord.F7, CurrentRecord.I7,
    DictSpecies, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[8], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[7], CurrentRecord.F7]);
  Result := Result + StringValidateField(CurrentRecord.F8, CurrentRecord.I8,
    DictSpecies, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[9], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[58], CurrentRecord.F58]);
  Result := Result + StringValidateField(CurrentRecord.F9, CurrentRecord.I9,
    DictDamage, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[58], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[9], CurrentRecord.F9]);
  Result := Result + StringValidateField(CurrentRecord.F58, CurrentRecord.I58,
    DictPest, Prompt);

  Result := Result + RelationValidateRecord(CurrentRecord, ReportYear);
end;

end.

