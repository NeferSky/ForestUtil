unit Script;

interface

uses
  Classes, Data;

type
  TDBScript = class(TObject)
  private
    FLines: TStringList;
  public
    procedure Clear;
    procedure SetHeader;
    procedure AddInsert(Values: TValuesRec);
    procedure SetFooter;
    function GetText: AnsiString;
  end;

implementation

//---------------------------------------------------------------------------
{ TDBScript }

procedure TDBScript.AddInsert(Values: TValuesRec);
var
  ValuesLine, SQLLine: AnsiString;

begin
  ValuesLine := Format('',
    [Values.F1, Values.F2, Values.F3, Values.F4, Values.F5, Values.F6,
    Values.F7, Values.F8, Values.F9, Values.F10, Values.F11, Values.F12,
    Values.F13, Values.F14, Values.F15, Values.F16, Values.F17, Values.F18,
    Values.F19, Values.F20, Values.F21, Values.F22, Values.F23, Values.F24,
    Values.F25, Values.F26, Values.F27, Values.F28, Values.F29, Values.F30,
    Values.F31, Values.F32, Values.F33, Values.F34, Values.F35, Values.F36,
    Values.F37, Values.F38, Values.F39, Values.F40, Values.F41, Values.F42,
    Values.F43, Values.F44, Values.F45, Values.F46, Values.F47, Values.F48,
    Values.F49, Values.F50, Values.F51, Values.F52, Values.F53, Values.F54,
    Values.F55, Values.F56, Values.F57, Values.F58, Values.F59, Values.F60,
    Values.F61, Values.F62, Values.F63, Values.F64, Values.F65, Values.F66,
    Values.F67, Values.F68]);

  SQLLine := Format('insert into Table (id, value) values (%s);', [ValuesLine]);
  FLines.Append(SQLLine);
end;

//---------------------------------------------------------------------------

procedure TDBScript.Clear;
begin
  FLines.Clear();
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

procedure TDBScript.SetFooter;
begin
  FLines.Append(S_SCRIPT_FOOTER);
end;

//---------------------------------------------------------------------------

procedure TDBScript.SetHeader;
begin
  FLines.Insert(0, S_SCRIPT_HEADER);
end;

end.
 