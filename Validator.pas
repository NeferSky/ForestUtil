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
    FLogDetails: TLogDetails;
    FValidationResult: TValidationResult;

    procedure AddRecordStatus(const Status: AnsiString; const ValidationID:
      TLogDetail);
    procedure AddValidationResult(const ValResult: TValidationRes);
    procedure StringValidateField(var Field: AnsiString; var FieldID: Integer;
      const Dict: TDictionary; const Prompt: AnsiString);
    procedure RelationValidateRecord(var CurrentRecord: TValuesRec);
    procedure MathValidateRecord(var CurrentRecord: TValuesRec);
    procedure MathExtraValidateRecord(var CurrentRecord: TValuesRec);
    procedure StringValidateRecord(var CurrentRecord: TValuesRec);
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    procedure InitCheck(LogDetails: TLogDetails);
    function GetDictionary(DictCaption: AnsiString): TDictionary;

    procedure Validate(const RecNo: Integer; var CurrentRecord: TValuesRec);

    property ValidationResult: TValidationResult read FValidationResult;
    property RecordStatus: AnsiString read FRecordStatus;
  end;

implementation

uses
  SysUtils, Edit, ForestConsts, Data;

//---------------------------------------------------------------------------
{ TValidator }

procedure TValidator.AddRecordStatus(const Status: AnsiString; const
  ValidationID: TLogDetail);
begin
  case ValidationID of
    ldMathErrors:
      begin
        if ldMathErrors in FLogDetails then
          FRecordStatus := FRecordStatus + Status;
      end;
    ldDictReplaces:
      begin
        if ldDictReplaces in FLogDetails then
          FRecordStatus := FRecordStatus + Status;
      end;
    ldRelationErrors:
      begin
        if ldRelationErrors in FLogDetails then
          FRecordStatus := FRecordStatus + Status;
      end;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.AddValidationResult(const ValResult: TValidationRes);
begin
  Include(FValidationResult, ValResult);
end;

//---------------------------------------------------------------------------

constructor TValidator.Create(Owner: TObject);
begin
  DictForestries := TDictionary.Create(S_DICT_FORESTRIES_FILE, S_DICT_FORESTRIES_NAME);
  DictForestries.SQLScript := S_SQL_GET_FORESTRIES_DICT;

  DictLocalForestries := TDictionary.Create(S_DICT_LOCAL_FORESTRIES_FILE, S_DICT_LOCAL_FORESTRIES_NAME);
  DictLocalForestries.SQLScript := S_SQL_GET_LOCAL_FORESTRIES_DICT;

  DictLanduse := TDictionary.Create(S_DICT_LANDUSE_FILE, S_DICT_LANDUSE_NAME);
  DictLanduse.SQLScript := S_SQL_GET_LANDUSE_DICT;

  DictProtectCategory := TDictionary.Create(S_DICT_PROTECT_CATEGORY_FILE, S_DICT_PROTECT_CATEGORY_NAME);
  DictProtectCategory.SQLScript := S_SQL_GET_PROTECT_CATEGORY_DICT;

  DictSpecies := TDictionary.Create(S_DICT_SPECIES_FILE, S_DICT_SPECIES_NAME);
  DictSpecies.SQLScript := S_SQL_GET_SPECIES_DICT;

  DictDamage := TDictionary.Create(S_DICT_DAMAGE_FILE, S_DICT_DAMAGE_NAME);
  DictDamage.SQLScript := S_SQL_GET_DAMAGE_DICT;

  DictPest := TDictionary.Create(S_DICT_PEST_FILE, S_DICT_PEST_NAME);
  DictPest.SQLScript := S_SQL_GET_PEST_DICT;
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

function TValidator.GetDictionary(DictCaption: AnsiString): TDictionary;
begin
  if DictCaption = S_DICT_FORESTRIES_NAME then
    Result := DictForestries
  else if DictCaption = S_DICT_LOCAL_FORESTRIES_NAME then
    Result := DictLocalForestries
  else if DictCaption = S_DICT_LANDUSE_NAME then
    Result := DictLanduse
  else if DictCaption = S_DICT_PROTECT_CATEGORY_NAME then
    Result := DictProtectCategory
  else if DictCaption = S_DICT_SPECIES_NAME then
    Result := DictSpecies
  else if DictCaption = S_DICT_DAMAGE_NAME then
    Result := DictDamage
  else if DictCaption = S_DICT_PEST_NAME then
    Result := DictPest;
end;

//---------------------------------------------------------------------------

procedure TValidator.InitCheck(LogDetails: TLogDetails);
begin
  DictForestries.ForceSkip := False;
  DictLocalForestries.ForceSkip := False;
  DictLanduse.ForceSkip := False;
  DictProtectCategory.ForceSkip := False;
  DictSpecies.ForceSkip := False;
  DictDamage.ForceSkip := False;
  DictPest.ForceSkip := False;

  FLogDetails := LogDetails;
end;

//---------------------------------------------------------------------------

procedure TValidator.MathExtraValidateRecord(var CurrentRecord: TValuesRec);
begin
  if CurrentRecord.F59 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 59, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F60 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 60, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F61 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 61, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F62 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 62, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F63 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 63, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F64 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 64, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F65 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 65, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F66 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 66, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F67 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 67, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F68 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrExtraInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 68, 13]),
      ldMathErrors);
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.MathValidateRecord(var CurrentRecord: TValuesRec);
var
  Tmp: Currency;

begin
  // 2
  if CurrentRecord.F13 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 13, 12]),
      ldMathErrors);
  end;

  // 3
  if (CurrentRecord.F14 + CurrentRecord.F15 + CurrentRecord.F16 <>
    CurrentRecord.F13) then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_SUM_ONE_DIFF_THAN_TWO, [FRecNo, '14-16', 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F14 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 14, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F15 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 15, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F16 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 16, 13]),
      ldMathErrors);
  end;

  // 4
  if CurrentRecord.F17 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 17, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F18 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 18, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F19 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 19, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F20 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 20, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F21 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 21, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F22 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 22, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F23 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 23, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F24 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 24, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F25 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 25, 13]),
      ldMathErrors);
  end;
  if CurrentRecord.F26 > CurrentRecord.F13 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 26, 13]),
      ldMathErrors);
  end;

  // 5
  if (CurrentRecord.F17 + CurrentRecord.F18 - CurrentRecord.F34 -
    CurrentRecord.F40 - CurrentRecord.F46 - CurrentRecord.F53) <> CurrentRecord.F19
    then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO, [FRecNo,
      '17+18-34-40-46-53', 19]), ldMathErrors);
  end;

  // 6
  if (CurrentRecord.F20 + CurrentRecord.F21 + CurrentRecord.F22 +
    CurrentRecord.F23) <> CurrentRecord.F19 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_SUM_ONE_DIFF_THAN_TWO, [FRecNo, '20-23', 19]),
      ldMathErrors);
  end;

  // 7
  if CurrentRecord.F24 > CurrentRecord.F17 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 24, 17]),
      ldMathErrors);
  end;
  if CurrentRecord.F25 > CurrentRecord.F18 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 25, 18]),
      ldMathErrors);
  end;
  if CurrentRecord.F26 > CurrentRecord.F19 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 26, 19]),
      ldMathErrors);
  end;

  // 8
  Tmp := CurrentRecord.F17 + CurrentRecord.F18;
  if CurrentRecord.F27 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 27, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F28 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 28, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F29 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 29, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F31 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 31, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F32 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 32, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F33 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 33, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F34 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 34, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F35 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 35, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F38 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 38, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F39 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 39, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F40 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 40, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F41 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 41, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F44 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 44, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F45 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 45, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F46 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 46, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F47 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 47, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F51 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 51, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F52 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 52, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F53 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 53, '17-18']),
      ldMathErrors);
  end;
  if CurrentRecord.F54 > Tmp then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_DIFF_THAN_SUM_TWO, [FRecNo, 54, '17-18']),
      ldMathErrors);
  end;

  //9
  if CurrentRecord.F32 > CurrentRecord.F27 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 32, 27]),
      ldMathErrors);
  end;
  if CurrentRecord.F33 > CurrentRecord.F27 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 33, 27]),
      ldMathErrors);
  end;
  if CurrentRecord.F34 > CurrentRecord.F27 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 34, 27]),
      ldMathErrors);
  end;
  if CurrentRecord.F35 > CurrentRecord.F27 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 35, 27]),
      ldMathErrors);
  end;
  if CurrentRecord.F38 > CurrentRecord.F28 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 38, 28]),
      ldMathErrors);
  end;
  if CurrentRecord.F39 > CurrentRecord.F28 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 39, 28]),
      ldMathErrors);
  end;
  if CurrentRecord.F40 > CurrentRecord.F28 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 40, 28]),
      ldMathErrors);
  end;
  if CurrentRecord.F41 > CurrentRecord.F28 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 41, 28]),
      ldMathErrors);
  end;
  if CurrentRecord.F44 > CurrentRecord.F29 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 44, 29]),
      ldMathErrors);
  end;
  if CurrentRecord.F45 > CurrentRecord.F29 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 45, 29]),
      ldMathErrors);
  end;
  if CurrentRecord.F46 > CurrentRecord.F29 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 46, 29]),
      ldMathErrors);
  end;
  if CurrentRecord.F47 > CurrentRecord.F29 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 47, 29]),
      ldMathErrors);
  end;
  if CurrentRecord.F51 > CurrentRecord.F31 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 51, 31]),
      ldMathErrors);
  end;
  if CurrentRecord.F52 > CurrentRecord.F31 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 52, 31]),
      ldMathErrors);
  end;
  if CurrentRecord.F53 > CurrentRecord.F31 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 53, 31]),
      ldMathErrors);
  end;
  if CurrentRecord.F54 > CurrentRecord.F31 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 54, 31]),
      ldMathErrors);
  end;

  //10
  if CurrentRecord.F32 > CurrentRecord.F34 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 32, 34]),
      ldMathErrors);
  end;
  if CurrentRecord.F33 > CurrentRecord.F35 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 33, 35]),
      ldMathErrors);
  end;

  //11
  if CurrentRecord.F38 > CurrentRecord.F40 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 38, 40]),
      ldMathErrors);
  end;
  if CurrentRecord.F39 > CurrentRecord.F41 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 39, 41]),
      ldMathErrors);
  end;

  //12
  if CurrentRecord.F44 > CurrentRecord.F46 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 44, 46]),
      ldMathErrors);
  end;
  if CurrentRecord.F45 > CurrentRecord.F47 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 45, 47]),
      ldMathErrors);
  end;

  //13
  if CurrentRecord.F51 > CurrentRecord.F53 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 51, 53]),
      ldMathErrors);
  end;
  if CurrentRecord.F52 > CurrentRecord.F54 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 52, 54]),
      ldMathErrors);
  end;

  //14
  if CurrentRecord.F33 > CurrentRecord.F32 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 33, 32]),
      ldMathErrors);
  end;
  if CurrentRecord.F35 > CurrentRecord.F34 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 35, 34]),
      ldMathErrors);
  end;
  if CurrentRecord.F39 > CurrentRecord.F38 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 39, 38]),
      ldMathErrors);
  end;
  if CurrentRecord.F41 > CurrentRecord.F40 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 41, 40]),
      ldMathErrors);
  end;
  if CurrentRecord.F45 > CurrentRecord.F44 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 45, 44]),
      ldMathErrors);
  end;
  if CurrentRecord.F47 > CurrentRecord.F46 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 47, 46]),
      ldMathErrors);
  end;
  if CurrentRecord.F52 > CurrentRecord.F51 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 52, 51]),
      ldMathErrors);
  end;
  if CurrentRecord.F54 > CurrentRecord.F53 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 54, 53]),
      ldMathErrors);
  end;

  //15
  if (CurrentRecord.F59 + CurrentRecord.F60 - CurrentRecord.F61 -
    CurrentRecord.F62) > CurrentRecord.F63 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO, [FRecNo,
      '59+60-61-62', 63]), ldMathErrors);
  end;

  //16
  if CurrentRecord.F64 > CurrentRecord.F63 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 64, 63]),
      ldMathErrors);
  end;

  // 17
  if (CurrentRecord.F65 + CurrentRecord.F66 + CurrentRecord.F67 +
    CurrentRecord.F68 <> CurrentRecord.F63) then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_SUM_ONE_DIFF_THAN_TWO, [FRecNo, '65-68', 63]),
      ldMathErrors);
  end;

  //18
  if CurrentRecord.F59 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 59, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F60 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 60, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F61 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 61, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F62 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 62, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F63 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 63, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F64 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 64, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F65 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 65, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F66 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 66, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F67 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 67, 12]),
      ldMathErrors);
  end;
  if CurrentRecord.F68 > CurrentRecord.F12 then
  begin
    AddValidationResult(vrMainInvalid);
    AddRecordStatus(Format(S_LOG_ONE_MORE_THAN_TWO, [FRecNo, 68, 12]),
      ldMathErrors);
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.RelationValidateRecord(var CurrentRecord: TValuesRec);
begin
  // Species relation check
  if dmData.GetIntField(Format(S_DB_GET_SPECIES_RELATION,
    [CurrentRecord.DamageSpeciesName, CurrentRecord.DamageReasonName])) <= 0 then
  begin
    AddValidationResult(vrRelationInvalid);
    AddRecordStatus(Format(S_LOG_NO_SPECIES_RELATION, [FRecNo]),
      ldRelationErrors);
  end;

  case CurrentRecord.DamageReasonID of
    881, 882, 883, 870, 871, 872, 873, 874, 875:
      if CurrentRecord.F10 <> CurrentRecord.ReportYear then
      begin
        AddValidationResult(vrRelationInvalid);
        AddRecordStatus(Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]),
          ldRelationErrors);
      end;
    821, 822, 823:
      if CurrentRecord.F10 >= CurrentRecord.ReportYear then
      begin
        AddValidationResult(vrRelationInvalid);
        AddRecordStatus(Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]),
          ldRelationErrors);
      end;
    851, 854, 856, 863, 864, 865:
      if (CurrentRecord.F10 > CurrentRecord.ReportYear - 1) or (CurrentRecord.F10
        < CurrentRecord.ReportYear - 3) then
      begin
        AddValidationResult(vrRelationInvalid);
        AddRecordStatus(Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]),
          ldRelationErrors);
      end;
    852, 855, 857, 866, 867, 868:
      if (CurrentRecord.F10 > CurrentRecord.ReportYear - 4) or (CurrentRecord.F10
        < CurrentRecord.ReportYear - 10) then
      begin
        AddValidationResult(vrRelationInvalid);
        AddRecordStatus(Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]),
          ldRelationErrors);
      end;
    853, 858, 869:
      if (CurrentRecord.F10 > CurrentRecord.ReportYear - 10) then
      begin
        AddValidationResult(vrRelationInvalid);
        AddRecordStatus(Format(S_LOG_NO_CAUSE_RELATION, [FRecNo]),
          ldRelationErrors);
      end;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.StringValidateField(var Field: AnsiString; var FieldID:
  Integer; const Dict: TDictionary; const Prompt: AnsiString);
var
  CatalogRecord: TCatalogRecord;
  Magic: AnsiString;
  WordIndex: Integer;

begin
  if Dict.ForceSkip then
  begin
    AddValidationResult(vrSkip);
    Exit;
  end;

  // Empty string - ask to replace and not remember
  if Trim(Field) = '' then
  begin
    case ShowEdit(Field, Prompt, Dict) of
      srReplace:
        begin
          Field := frmEdit.CurrentText;
          FieldID := frmEdit.CurrentIndex;
        end;
      srForceSkip:
        begin
          AddValidationResult(vrSkip);
          Dict.ForceSkip := True;
        end;
      srSkip:
        AddValidationResult(vrSkip);
      srStop:
        AddValidationResult(vrStop);
    end;

    Exit;
  end;

  WordIndex := Dict.Validate(Field);
  if WordIndex <> -1 then
    // Word is correct and full-valid
    FieldID := WordIndex
  else
  begin
    // Found in dictionary - autoreplace, log only
    if Dict.FindRecord(Field, CatalogRecord) then
    begin
      AddValidationResult(vrStringInvalid);
      AddRecordStatus(Format(S_LOG_REPLACE_FROM_DICTIONARY, [FRecNo,
        CatalogRecord.NewWord, Field]), ldDictReplaces);
    end

      // Not found in dictionary - ask to replace, remember and log
    else
    begin
      // here ask...
      case ShowEdit(Field, Prompt, Dict) of
        srReplace:
          begin
            // ...here remember...
            CatalogRecord.OldWord := Field;
            CatalogRecord.NewWord := frmEdit.CurrentText;
            CatalogRecord.NewIndex := frmEdit.CurrentIndex;
            Dict.WriteRecord(CatalogRecord);
            // ...here log...
            AddValidationResult(vrStringInvalid);
            AddRecordStatus(Format(S_LOG_REPLACE_FROM_DICTIONARY, [FRecNo,
              CatalogRecord.NewWord, Field]), ldDictReplaces);
            // ...here replace
            Field := frmEdit.cmbSynonim.Text;
            FieldID := frmEdit.CurrentIndex;
          end;
        srStop:
          begin
            AddValidationResult(vrStop);
            Exit;
          end;
        srSkip:
          begin
            AddValidationResult(vrSkip);
            Exit;
          end;
        srForceSkip:
          begin
            AddValidationResult(vrSkip);
            Dict.ForceSkip := True;
          end;
      end;
    end;

    // Some magic to set new word into variable
    FieldID := CatalogRecord.NewIndex;
    Magic := CatalogRecord.NewWord;
    Field := Magic;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.StringValidateRecord(var CurrentRecord: TValuesRec);
var
  Prompt: AnsiString;

begin
  // LocalForestries - validate always
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[2], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[1], CurrentRecord.ForestryName]);
  StringValidateField(CurrentRecord.LocalForestryName, CurrentRecord.LocalForestryID,
    DictLocalForestries, Prompt);
  if vrStop in FValidationResult then
    Exit;

  // LanduseName - validate always
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[5], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[6], CurrentRecord.DefenseCategoryName]);
  StringValidateField(CurrentRecord.LanduseName, CurrentRecord.LanduseID,
    DictLanduse, Prompt);
  if vrStop in FValidationResult then
    Exit;

  // DefenseCategoryName - validate only if LanduseID <> 2 or if
  // non-empty for LaduseID = 2
  // wherein, if result (DefenseCategoryID) = 120 some next fields must be 0
  if (CurrentRecord.LanduseID <> 2) or ((CurrentRecord.LanduseID = 2) and
    (CurrentRecord.DefenseCategoryName <> '')) then
  begin
    StringValidateField(CurrentRecord.DefenseCategoryName,
      CurrentRecord.DefenseCategoryID, DictProtectCategory, ARR_FIELD_NAMES[6]);
    if vrStop in FValidationResult then
      Exit;

    if CurrentRecord.DefenseCategoryID = 120 then
    begin
      CurrentRecord.F27 := 0;
      CurrentRecord.F32 := 0;
      CurrentRecord.F33 := 0;
      CurrentRecord.F34 := 0;
      CurrentRecord.F35 := 0;
      CurrentRecord.F36 := 0;
      CurrentRecord.F37 := 0;
    end;
  end;

  // MainSpeciesName - validate always
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[7], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[8], CurrentRecord.DamageSpeciesName]);
  StringValidateField(CurrentRecord.MainSpeciesName,
    CurrentRecord.MainSpeciesID, DictSpecies, Prompt);
  if vrStop in FValidationResult then
    Exit;

  // DamageSpeciesName - validate always
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[8], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[7], CurrentRecord.MainSpeciesName]);
  StringValidateField(CurrentRecord.DamageSpeciesName,
    CurrentRecord.DamageSpeciesID, DictSpecies, Prompt);
  if vrStop in FValidationResult then
    Exit;

  // DamageReasonName - validate always
  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[9], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[58], CurrentRecord.PestName]);
  StringValidateField(CurrentRecord.DamageReasonName,
    CurrentRecord.DamageReasonID, DictDamage, Prompt);
  if vrStop in FValidationResult then
    Exit;

  // PestName - validate if non-empty and if at least one of some fields is non-zero
  // else result (PestID) must be 0 and PestStatus must be ''
  if CurrentRecord.PestName <> '' then
  begin
    if (CurrentRecord.F59 = 0) and (CurrentRecord.F60 = 0) and (CurrentRecord.F61
      = 0) and (CurrentRecord.F62 = 0) and (CurrentRecord.F63 = 0) and
      (CurrentRecord.F64 = 0) and (CurrentRecord.F65 = 0) and (CurrentRecord.F66 =
      0) and (CurrentRecord.F67 = 0) and (CurrentRecord.F68 = 0) then
    begin
      CurrentRecord.PestStatus := '';
      CurrentRecord.PestID := 0;
    end

    else
    begin
      Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[58], S_EDIT_PROMPT,
        ARR_FIELD_NAMES[9], CurrentRecord.DamageReasonName]);
      StringValidateField(CurrentRecord.PestName, CurrentRecord.PestID,
        DictPest, Prompt);
      if vrStop in FValidationResult then
        Exit;
    end;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.Validate(const RecNo: Integer; var CurrentRecord:
  TValuesRec);
begin
  FValidationResult := [];
  FRecordStatus := '';
  FRecNo := RecNo;

  StringValidateRecord(CurrentRecord);
  CurrentRecord.ValidationResult := FValidationResult;
  if vrStop in FValidationResult then
    Exit;

  RelationValidateRecord(CurrentRecord);
  MathValidateRecord(CurrentRecord);
  MathExtraValidateRecord(CurrentRecord);
  CurrentRecord.ValidationResult := FValidationResult;
end;

end.

