unit DBScript;

interface

uses
  Classes, ForestConsts, ForestTypes;

type
  TDBScript = class(TObject)
  private
    FLines: TStringList;
    FRegionID: Integer;
    FForestryID: Integer;
    FLocalForestryID: Integer;
    FPestCode: Integer;
    FLandusePurposeCode: Integer;
    FProtectCategoryCode: Integer;
    FSpeciesID: Integer;
    FDamagedSpeciesID: Integer;
    FCauseCode: Integer;
    procedure SetLineHeader;
    procedure SetLineFooter;
    function GetNewID(const LandQuarter: AnsiString;
  const Land: AnsiString; const YearQuarter: Integer): AnsiString;
    procedure GetIDs(const LocalForestryName, PestName, LandusePurposeName,
      ProtectCategoryName, SpeciesName, DamagedSpeciesName,
      CauseName: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetScriptHeader;
    procedure AddDelete(const RegionID, ForestryID, ReportQuarter,
      ReportYear: Integer);
    procedure AddInsert(Values: TValuesRec; const RegionID, ForestryID,
      ReportQuarter, ReportYear: Integer);
    procedure SetScriptFooter;
    function GetText: AnsiString;
  end;

implementation

uses
  SysUtils, Data, NsUtils;

//---------------------------------------------------------------------------
{ TDBScript }

procedure TDBScript.AddDelete(const RegionID, ForestryID, ReportQuarter,
  ReportYear: Integer);
var
  SQLLine: AnsiString;

begin
  SQLLine := Format(S_DB_DELETE_SCRIPT_FORMAT, [S_DB_TABLE_NAME, ForestryID,
    ReportQuarter, ReportYear]);

  SetLineHeader();
  FLines.Append('-- Удаление строк отчета');
  FLines.Append(SQLLine);
  SetLineFooter();
end;
 
//---------------------------------------------------------------------------

procedure TDBScript.AddInsert(Values: TValuesRec; const RegionID,
  ForestryID, ReportQuarter, ReportYear: Integer);

  function RepairDot(const C: Currency): AnsiString;
  begin
    Result := FloatToStr(C);
    Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
  end;

var
  SQLLine: AnsiString;
  DateStr: AnsiString;
  NewID: AnsiString;

begin
  DateTimeToString(DateStr, 'dd.mm.yyyy', Date());
  GetIDs(Values.F2, Values.F58, Values.F5, Values.F6, Values.F7, Values.F8,
    Values.F9);
  FRegionID := RegionID;
  FForestryID := ForestryID;
  NewID := GetNewID(IntToStr(Values.F3), IntToStr(Values.F4), ReportQuarter);

  SQLLine := Format(
    S_DB_INSERT_SCRIPT_FORMAT_BEGIN +
    S_DB_INSERT_SCRIPT_FORMAT_FLD1 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD2 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD3 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD4 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD5 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD6 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD7 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD8 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD9 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD10 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD11 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD12 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD13 +
    S_DB_INSERT_SCRIPT_FORMAT_FLD14 +
    S_DB_INSERT_SCRIPT_FORMAT_MIDDLE +
    S_DB_INSERT_SCRIPT_FORMAT_VAL1 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL2 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL3 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL4 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL5 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL6 +
    S_DB_INSERT_SCRIPT_FORMAT_VAL7 +
    S_DB_INSERT_SCRIPT_FORMAT_END,

    [S_DB_TABLE_NAME, NewID, DateStr, ReportQuarter, ReportYear,
    FForestryID, Values.I2, Values.F3,
      Values.F4, Values.I5, Values.I6,
      Values.I7, Values.I8, Values.I9,
      Values.F10, Values.F11, RepairDot(Values.F12),
      RepairDot(Values.F13), RepairDot(Values.F14), RepairDot(Values.F15),
      RepairDot(Values.F16), RepairDot(Values.F17), RepairDot(Values.F18),
      RepairDot(Values.F19), RepairDot(Values.F20), RepairDot(Values.F21),
      RepairDot(Values.F22), RepairDot(Values.F23), RepairDot(Values.F24),
      RepairDot(Values.F25), RepairDot(Values.F26), RepairDot(Values.F27),
      RepairDot(Values.F28), RepairDot(Values.F29), Values.F30,
      RepairDot(Values.F31), RepairDot(Values.F32), RepairDot(Values.F33),
      RepairDot(Values.F34), RepairDot(Values.F35), RepairDot(Values.F36),
      RepairDot(Values.F37), RepairDot(Values.F38), RepairDot(Values.F39),
      RepairDot(Values.F40), RepairDot(Values.F41), RepairDot(Values.F42),
      RepairDot(Values.F43), RepairDot(Values.F44), RepairDot(Values.F45),
      RepairDot(Values.F46), RepairDot(Values.F47), RepairDot(Values.F48),
      RepairDot(Values.F49), Values.F50, RepairDot(Values.F51),
      RepairDot(Values.F52), RepairDot(Values.F53), RepairDot(Values.F54),
      RepairDot(Values.F55), RepairDot(Values.F56), Values.F57,
      Values.I58, RepairDot(Values.F59), RepairDot(Values.F60),
      RepairDot(Values.F61), RepairDot(Values.F62), RepairDot(Values.F63),
      RepairDot(Values.F64), RepairDot(Values.F65), RepairDot(Values.F66),
      RepairDot(Values.F67), RepairDot(Values.F68)]
      );

  SetLineHeader();
  FLines.Append('-- Вставка строки нового отчета');
  FLines.Append(SQLLine);
  SetLineFooter();
end;

//---------------------------------------------------------------------------

procedure TDBScript.Clear;
begin
  FLines.Clear();
end;

//---------------------------------------------------------------------------

constructor TDBScript.Create;
begin
  inherited;

  FLines := TStringList.Create();
end;

//---------------------------------------------------------------------------

destructor TDBScript.Destroy;
begin
  FLines.Free();

  inherited;
end;

//---------------------------------------------------------------------------

procedure TDBScript.GetIDs(const LocalForestryName, PestName,
  LandusePurposeName, ProtectCategoryName, SpeciesName, DamagedSpeciesName,
  CauseName: AnsiString);
begin

{  FLocalForestryID := dmData.GetIntField(Format(
    S_DB_GET_LOCAL_FORESTRY_ID, [LocalForestryName]));
  FPestCode := dmData.GetIntField(Format(
    S_DB_GET_PEST_CODE, [PestName]));
  FLandusePurposeCode := dmData.GetIntField(Format(
    S_DB_GET_LANDUSE_PURPOSE_CODE, [LandusePurposeName]));
  FProtectCategoryCode := dmData.GetIntField(Format(
    S_DB_GET_PROTECT_CATEGORY_CODE, [ProtectCategoryName]));
  FSpeciesID := dmData.GetIntField(Format(
    S_DB_GET_SPECIES_ID, [SpeciesName]));
  FDamagedSpeciesID := dmData.GetIntField(Format(
    S_DB_GET_SPECIES_ID, [DamagedSpeciesName]));
  FCauseCode := dmData.GetIntField(Format(
    S_DB_GET_CAUSE_CODE, [CauseName]));
    }
end;

//---------------------------------------------------------------------------

function TDBScript.GetNewID(const LandQuarter: AnsiString;
  const Land: AnsiString; const YearQuarter: Integer): AnsiString;
var
  ADay, AMonth, AYear: Word;

begin
  Result := '';

  DecodeDate(Date(), AYear, AMonth, ADay);

  Result := Result + ExtendLeft(IntToStr(FRegionID), 3, '0');
  Result := Result + ExtendLeft(IntToStr(FForestryID), 3, '0');
  Result := Result + ExtendLeft(IntToStr(FLocalForestryID), 3, '0');
  Result := Result + ExtendLeft(LandQuarter, 3, '0');
  Result := Result + ExtendLeft(Land, 3, '0');

  Result := Result + IntToStr(YearQuarter);

  Result := Result + ExtendLeft(IntToStr(AMonth), 3, '0');
  Result := Result + ExtendLeft(IntToStr(AYear), 3, '0');
end;

//---------------------------------------------------------------------------

function TDBScript.GetText: AnsiString;
var
  I: Integer;

begin
  Result := '';

  for I := 0 to FLines.Count - 1 do
  begin
    Result := Result + #13#10 + FLines[I];
  end;
end;

//---------------------------------------------------------------------------

procedure TDBScript.SetLineFooter;
begin
  if S_DB_SCRIPT_LN_FOOTER <> '' then
    FLines.Append(S_DB_SCRIPT_LN_FOOTER);
end;

//---------------------------------------------------------------------------

procedure TDBScript.SetLineHeader;
begin
  if S_DB_SCRIPT_LN_HEADER <> '' then
    FLines.Append(S_DB_SCRIPT_LN_HEADER);
end;

//---------------------------------------------------------------------------

procedure TDBScript.SetScriptFooter;
begin
  if S_DB_SCRIPT_FOOTER <> '' then
    FLines.Append(S_DB_SCRIPT_FOOTER);
end;

//---------------------------------------------------------------------------

procedure TDBScript.SetScriptHeader;
begin
  if S_DB_SCRIPT_HEADER <> '' then
    FLines.Insert(0, S_DB_SCRIPT_HEADER);
end;

end.

