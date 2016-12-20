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
    FForceSkipLocalForestries: Boolean;
    FForceSkipLanduse: Boolean;
    FForceSkipProtectCategory: Boolean;
    FForceSkipSpecies: Boolean;
    FForceSkipDamagedSpecies: Boolean;
    FForceSkipDamage: Boolean;
    FForceSkipPest: Boolean;
    function MainValidateRecord(const RecNo: Integer; var Values: TValuesRec;
      var RecordStatus: string): TValidationResult;
    function ExtraValidateRecord(const RecNo: Integer; var Values: TValuesRec;
      var RecordStatus: string): TValidationResult;
    function StringValidateField(var Field: AnsiString;
      const Dict: TDictionary; const RecNo: Integer;
      var RecordStatus: string; var ForceSkip: Boolean;
      const Prompt: AnsiString): TValidationResult;
    function RelationValidate(const RecNo: Integer; const Values: TValuesRec;
      var RecordStatus: AnsiString; const ReportYear: Integer): TValidationResult;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    procedure InitCheck;
    function MathValidateRecord(const RecNo: Integer; var Values: TValuesRec;
      var RecordStatus: AnsiString): TValidationResult;
    function StringValidateRecord(const RecNo: Integer; var Values: TValuesRec;
      var RecordStatus: AnsiString; const ReportYear: Integer): TValidationResult;
    function GetValidList(Dictionary: AnsiString): TStringList;
    procedure SetValidList(Dictionary: AnsiString; Value: TStringList);
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

function TValidator.ExtraValidateRecord(const RecNo: Integer;
  var Values: TValuesRec; var RecordStatus: string): TValidationResult;
begin
  Result := [vrExtraValid];

  if Values.F59 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 59, 13]);
  end;
  if Values.F60 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 60, 13]);
  end;
  if Values.F61 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 61, 13]);
  end;
  if Values.F62 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 62, 13]);
  end;
  if Values.F63 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 63, 13]);
  end;
  if Values.F64 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 64, 13]);
  end;
  if Values.F65 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 65, 13]);
  end;
  if Values.F66 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 66, 13]);
  end;
  if Values.F67 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 67, 13]);
  end;
  if Values.F68 > Values.F13 then
  begin
    Result := [vrExtraInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
      [RecNo, 68, 13]);
  end;
end;

//---------------------------------------------------------------------------

function TValidator.GetValidList(Dictionary: AnsiString): TStringList;
begin
  if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_FORESTRIES_FILE) then
    Result := DictForestries.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LOCAL_FORESTRIES_FILE) then
    Result := DictLocalForestries.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_LANDUSE_FILE) then
    Result := DictLanduse.ValidList
     
  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PROTECT_CATEGORY_FILE) then
    Result := DictProtectCategory.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_SPECIES_FILE) then
    Result := DictSpecies.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_DAMAGE_FILE) then
    Result := DictDamage.ValidList

  else if Dictionary = (S_DICTIONARY_VALID_PREFIX +
    S_DICTIONARY_PEST_FILE) then
    Result := DictPest.ValidList;
end;

//---------------------------------------------------------------------------

procedure TValidator.InitCheck;
begin
  FForceSkipLocalForestries := False;
  FForceSkipLanduse := False;
  FForceSkipProtectCategory := False;
  FForceSkipSpecies := False;
  FForceSkipDamagedSpecies := False;
  FForceSkipDamage := False;
  FForceSkipPest := False;
end;

//---------------------------------------------------------------------------

function TValidator.MainValidateRecord(const RecNo: Integer;
  var Values: TValuesRec; var RecordStatus: string): TValidationResult;
var
  Tmp: Currency;

begin
  Result := [vrMainValid];
  RecordStatus := '';

  // 2
  if Values.F13 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 13, 12]);
  end;

  // 3
  if (Values.F14 + Values.F15 + Values.F16
    <> Values.F13) then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [RecNo, '14-16', 12]);
  end;

  // 4
  if Values.F17 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 17, 13]);
  end;
  if Values.F18 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 18, 13]);
  end;
  if Values.F19 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 19, 13]);
  end;
  if Values.F20 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 20, 13]);
  end;
  if Values.F21 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 21, 13]);
  end;
  if Values.F22 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 22, 13]);
  end;
  if Values.F23 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 23, 13]);
  end;
  if Values.F24 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 24, 13]);
  end;
  if Values.F25 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 25, 13]);
  end;
  if Values.F26 > Values.F13 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 26, 13]);
  end;

  // 5
  if (Values.F17 + Values.F18 - Values.F34 - Values.F40 - Values.F46 -
    Values.F53) <> Values.F19 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO,
      [RecNo, '17+18-34-40-46-53', 19]);
  end;

  // 6
  if (Values.F20 + Values.F21 + Values.F22 +
    Values.F23) <> Values.F19 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [RecNo, '20-23', 19]);
  end;

  // 7
  if Values.F24 > Values.F17 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 24, 17]);
  end;
  if Values.F25 > Values.F18 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 25, 18]);
  end;
  if Values.F26 > Values.F19 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 26, 19]);
  end;

  // 8
  Tmp := Values.F17 + Values.F18;
  if Values.F27 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 27, '17-18']);
  end;
  if Values.F28 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 28, '17-18']);
  end;
  if Values.F29 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 29, '17-18']);
  end;
  {  if Values.F30 > Tmp then
    begin
      Result := [vrMainInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
        [RecNo, 30, '17-18']);
    end;
    }
  if Values.F31 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
      [RecNo, 31, '17-18']);
  end;
  if Values.F32 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 32, '17-18']);
  end;
  if Values.F33 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 33, '17-18']);
  end;
  if Values.F34 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 34, '17-18']);
  end;
  if Values.F35 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 35, '17-18']);
  end;
  if Values.F38 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 38, '17-18']);
  end;
  if Values.F39 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 39, '17-18']);
  end;
  if Values.F40 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 40, '17-18']);
  end;
  if Values.F41 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 41, '17-18']);
  end;
  if Values.F44 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 44, '17-18']);
  end;
  if Values.F45 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 45, '17-18']);
  end;
  if Values.F46 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 46, '17-18']);
  end;
  if Values.F47 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 47, '17-18']);
  end;
  if Values.F51 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 51, '17-18']);
  end;
  if Values.F52 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 52, '17-18']);
  end;
  if Values.F53 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 53, '17-18']);
  end;
  if Values.F54 > Tmp then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_DIFF_THAN_SUM_TWO,
    [RecNo, 54, '17-18']);
  end;

  //9
  if Values.F32 > Values.F27 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 32, 27]);
  end;
  if Values.F33 > Values.F27 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 33, 27]);
  end;
  if Values.F34 > Values.F27 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 34, 27]);
  end;
  if Values.F35 > Values.F27 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 35, 27]);
  end;
  if Values.F38 > Values.F28 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 38, 28]);
  end;
  if Values.F39 > Values.F28 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 39, 28]);
  end;
  if Values.F40 > Values.F28 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 40, 28]);
  end;
  if Values.F41 > Values.F28 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 41, 28]);
  end;
  if Values.F44 > Values.F29 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 44, 29]);
  end;
  if Values.F45 > Values.F29 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 45, 29]);
  end;
  if Values.F46 > Values.F29 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 46, 29]);
  end;
  if Values.F47 > Values.F29 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 47, 29]);
  end;
  if Values.F51 > Values.F31 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 51, 31]);
  end;
  if Values.F52 > Values.F31 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 52, 31]);
  end;
  if Values.F53 > Values.F31 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 53, 31]);
  end;
  if Values.F54 > Values.F31 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 54, 31]);
  end;

  //10
  if Values.F32 > Values.F34 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 32, 34]);
  end;
  if Values.F33 > Values.F35 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 33, 35]);
  end;

  //11
  if Values.F38 > Values.F40 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 38, 40]);
  end;
  if Values.F39 > Values.F41 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 39, 41]);
  end;

  //12
  if Values.F44 > Values.F46 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 44, 46]);
  end;
  if Values.F45 > Values.F47 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 45, 47]);
  end;

  //13
  if Values.F51 > Values.F53 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 51, 53]);
  end;
  if Values.F52 > Values.F54 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 52, 54]);
  end;

  //14
  if Values.F33 > Values.F32 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 33, 32]);
  end;
  if Values.F35 > Values.F34 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 35, 34]);
  end;
  if Values.F39 > Values.F38 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 39, 38]);
  end;
  if Values.F41 > Values.F40 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 41, 40]);
  end;
  if Values.F45 > Values.F44 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 45, 44]);
  end;
  if Values.F47 > Values.F46 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 47, 46]);
  end;
  if Values.F52 > Values.F51 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 52, 51]);
  end;
  if Values.F54 > Values.F53 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 54, 53]);
  end;

  //15
  if (Values.F59 + Values.F60 - Values.F61 - Values.F62) > Values.F63 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_FORMULA_ONE_DIFF_THAN_TWO,
      [RecNo, '59+60-61-62', 63]);
  end;

  //16
  if Values.F64 > Values.F63 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 64, 63]);
  end;

  // 17
  if (Values.F65 + Values.F66 + Values.F67 +
    Values.F68 <> Values.F63) then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_SUM_ONE_DIFF_THAN_TWO,
    [RecNo, '65-68', 63]);
  end;

  //18
  if Values.F59 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 59, 12]);
  end;
  if Values.F60 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 60, 12]);
  end;
  if Values.F61 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 61, 12]);
  end;
  if Values.F62 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 62, 12]);
  end;
  if Values.F63 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 63, 12]);
  end;
  if Values.F64 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 64, 12]);
  end;
  if Values.F65 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 65, 12]);
  end;
  if Values.F66 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 66, 12]);
  end;
  if Values.F67 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 67, 12]);
  end;
  if Values.F68 > Values.F12 then
  begin
    Result := [vrMainInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_ONE_MORE_THAN_TWO,
    [RecNo, 68, 12]);
  end;
end;
    
//---------------------------------------------------------------------------

function TValidator.MathValidateRecord(const RecNo: Integer;
  var Values: TValuesRec; var RecordStatus: string): TValidationResult;
begin
  Result := [];
  RecordStatus := '';
  Result := Result + MainValidateRecord(RecNo, Values, RecordStatus);
  Result := Result + ExtraValidateRecord(RecNo, Values, RecordStatus);
end;

//---------------------------------------------------------------------------

function TValidator.RelationValidate(const RecNo: Integer;
  const Values: TValuesRec; var RecordStatus: AnsiString;
  const ReportYear: Integer): TValidationResult;
var
  CauseCode: Integer;
  AYear, AMonth, ADay: Word;

begin
  // Species relation check
  if not dmData.CheckSpeciesRelation(Values.F8, Values.F9) then
  begin
    Result := Result + [vrRelationInvalid];
    RecordStatus := RecordStatus + Format(S_LOG_NO_SPECIES_RELATION, [RecNo]);
  end;

  // Cause relation check
  CauseCode := dmData.GetIntField(Format(S_DB_GET_CAUSE_CODE_BY_NAME, [Values.F9]));

  case CauseCode of
  881, 882, 883, 870, 871, 872, 873, 874, 875:
    if Values.F10 <> ReportYear then
    begin
      Result := Result + [vrRelationInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [RecNo]);
    end;
  821, 822, 823:
    if Values.F10 >= ReportYear then
    begin
      Result := Result + [vrRelationInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [RecNo]);
    end;
  851, 854, 856, 863, 864, 865:
    if (Values.F10 > ReportYear - 1) or (Values.F10 < ReportYear - 3) then
    begin
      Result := Result + [vrRelationInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [RecNo]);
    end;
  852, 855, 857, 866, 867, 868:
    if (Values.F10 > ReportYear - 4) or (Values.F10 < ReportYear - 10) then
    begin
      Result := Result + [vrRelationInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [RecNo]);
    end;
  853, 858, 869:
    if (Values.F10 > ReportYear - 10) then
    begin
      Result := Result + [vrRelationInvalid];
      RecordStatus := RecordStatus + Format(S_LOG_NO_CAUSE_RELATION, [RecNo]);
    end;
  end;
end;

//---------------------------------------------------------------------------

procedure TValidator.SetValidList(Dictionary: AnsiString; Value: TStringList);
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
  const Dict: TDictionary; const RecNo: Integer;
  var RecordStatus: string; var ForceSkip: Boolean;
  const Prompt: AnsiString): TValidationResult;
var
  DictRecord: TDictRecord;
  Magic: AnsiString;

begin
  if ForceSkip then
    Exit;

  // Empty string - ask to replace and not remember
  if Trim(Field) = '' then
  begin
    case ShowEdit(Field, Prompt, Dict.ValidList) of
    srReplace:
      Field := frmEdit.cmbSynonim.Text;
    srForceSkip:
      ForceSkip := True;
    srStop:
      Result := Result + [vrStop];
    end;
    Exit;
  end;

  if not Dict.Validate(Field) then
  begin
    // Found in dictionary - autoreplace, log only
    if Dict.FindRecord(Field, DictRecord) then
    begin
      RecordStatus := RecordStatus + Format(S_LOG_REPLACE_FROM_DICTIONARY,
        [RecNo, DictRecord.NewWord, Field]);
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
        Dict.WriteRecord(DictRecord);
        // ...here log...
        RecordStatus := RecordStatus + Format(S_LOG_REPLACE_FROM_DICTIONARY,
          [RecNo, DictRecord.NewWord, Field]);
        // ...here replace
        Field := frmEdit.cmbSynonim.Text;
      end;

      srSkip:
        Exit;

      srForceSkip:
        ForceSkip := True;

      srStop:
      begin
        Result := Result + [vrStop];
        Exit;
      end;
      end;
    end;

    // Some magic to set new word into variable
    Magic := DictRecord.NewWord;
    Field := Magic;
  end;
end;

//---------------------------------------------------------------------------

function TValidator.StringValidateRecord(const RecNo: Integer;
  var Values: TValuesRec; var RecordStatus: AnsiString;
  const ReportYear: Integer): TValidationResult;
var
  Prompt: AnsiString;

begin
  Result := [];
  RecordStatus := '';
  
  //  Prompt := ARR_FIELD_NAMES[1] + #13#10 + '�������� ������� �� ������ �������:' +
  //    #13#10 + ARR_FIELD_NAMES[2] + ': ' + Values.F2;
  //  Result := Result + StringValidateField(Values.F1, DictForestries, RecNo,
  //    RecordStatus, FForceSkipForestries, Prompt);
  //  if vrStop in Result then
  //    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[2], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[1], Values.F1]);
  Result := Result + StringValidateField(Values.F2, DictLocalForestries, RecNo,
    RecordStatus, FForceSkipLocalForestries, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[5], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[6], Values.F6]);
  Result := Result + StringValidateField(Values.F5, DictLanduse, RecNo,
    RecordStatus, FForceSkipLanduse, Prompt);
  if vrStop in Result then
    Exit;
            
  Result := Result + StringValidateField(Values.F6, DictProtectCategory, RecNo,
    RecordStatus, FForceSkipProtectCategory, ARR_FIELD_NAMES[6]);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[7], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[8], Values.F8]);
  Result := Result + StringValidateField(Values.F7, DictSpecies, RecNo,
    RecordStatus, FForceSkipSpecies, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[8], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[7], Values.F7]);
  Result := Result + StringValidateField(Values.F8, DictSpecies, RecNo,
    RecordStatus, FForceSkipDamagedSpecies, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[9], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[58], Values.F58]);
  Result := Result + StringValidateField(Values.F9, DictDamage, RecNo,
    RecordStatus, FForceSkipDamage, Prompt);
  if vrStop in Result then
    Exit;

  Prompt := Format('%s%s%s: "%s"', [ARR_FIELD_NAMES[58], S_EDIT_PROMPT,
    ARR_FIELD_NAMES[9], Values.F9]);
  Result := Result + StringValidateField(Values.F58, DictPest, RecNo,
    RecordStatus, FForceSkipPest, Prompt);

  Result := Result + RelationValidate(RecNo, Values, RecordStatus, ReportYear);
end;

end.

