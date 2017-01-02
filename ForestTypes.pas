unit ForestTypes;

interface

type
  TOnValidationLog = procedure(Msg: AnsiString) of object;
  TOnProgress = procedure(CurrentIteration: Integer) of object;

type
  TQueryType = (qtSelect, qtCommand);

type
  TShowResult = (srReplace, srSkip, srForceSkip, srStop);

type
  TSQLQuery = record
    QueryIndex: Integer;
    QueryText: string[255];
  end;

type
  TValidationRes = (vrDuplicateInvalid, vrMainInvalid, vrExtraInvalid,
    vrStringInvalid, vrRelationInvalid, vrStop, vrSkip);
  TValidationResult = set of TValidationRes;

type
  TLogDetail = (ldMathErrors, ldDuplicates, ldRelationErrors, ldDictReplaces,
    ldEmptyRecords, ldPrevReportSum);
  TLogDetails = set of TLogDetail;

type
  TValuesRec = record
    {F1}ForestryName: AnsiString;
    {F1 ID}ForestryID: Integer;
    {F2}LocalForestryName: AnsiString;
    {F2 ID}LocalForestryID: Integer;
    Quarter: Integer;
    Patch: Integer;
    {F5}LanduseName: AnsiString;
    {F5 ID}LanduseID: Integer;
    {F6}DefenseCategoryName: AnsiString;
    {F6 ID}DefenseCategoryID: Integer;
    {F7}MainSpeciesName: AnsiString;
    {F7 ID}MainSpeciesID: Integer;
    {F8}DamageSpeciesName: AnsiString;
    {F8 ID}DamageSpeciesID: Integer;
    {F9}DamageReasonName: AnsiString;
    {F9 ID}DamageReasonID: Integer;
    F10: Integer;
    F11: Integer;
    F12: Currency;
    F13: Currency;
    F14: Currency;
    F15: Currency;
    F16: Currency;
    F17: Currency;
    F18: Currency;
    F19: Currency;
    F20: Currency;
    F21: Currency;
    F22: Currency;
    F23: Currency;
    F24: Currency;
    F25: Currency;
    F26: Currency;
    F27: Currency;
    F28: Currency;
    F29: Currency;
    F30: AnsiString;
    F31: Currency;
    F32: Currency;
    F33: Currency;
    F34: Currency;
    F35: Currency;
    F36: Currency;
    F37: Currency;
    F38: Currency;
    F39: Currency;
    F40: Currency;
    F41: Currency;
    F42: Currency;
    F43: Currency;
    F44: Currency;
    F45: Currency;
    F46: Currency;
    F47: Currency;
    F48: Currency;
    F49: Currency;
    F50: AnsiString;
    F51: Currency;
    F52: Currency;
    F53: Currency;
    F54: Currency;
    F55: Currency;
    F56: Currency;
    {F57}PestStatus: AnsiString;
    {F58}PestName: AnsiString;
    {F58 ID}PestID: Integer;
    F59: Currency;
    F60: Currency;
    F61: Currency;
    F62: Currency;
    F63: Currency;
    F64: Currency;
    F65: Currency;
    F66: Currency;
    F67: Currency;
    F68: Currency;
    RegionID: Integer;
    ReportQuarter: Integer;
    ReportYear: Integer;
    ValidationResult: TValidationResult;
  end;

type
  TConfirmResult = (crConfirmed, crSkip, crRemember, crStop);
  TConfirmResults = set of TConfirmResult;

type
  TUniqueKey = record
    RecNo: Integer;
    Forestry: AnsiString;
    LocalForestry: AnsiString;
    Quarter: Integer;
    Location: Integer;
    Landuse: AnsiString;
    LocationArea: Currency;
  end;

type
  TDictRecord = record
    OldWord: string[255];
    NewWord: string[255];
    NewIndex: Integer;
  end;

type
  TValidRecord = record
    WordIndex: Integer;
    WordValue: string[255];
  end;

type
  TValidArr = array of TValidRecord;

type
  TReportSums = record
    DamagedArea: Currency;
    LostArea: Currency;
    PestArea: Currency;
  end;

type
  TSkippedRecs = array of Integer;

implementation

end.

