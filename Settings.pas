unit Settings;

interface

uses
  SysUtils, Controls, Forms, StdCtrls, Grids, ValEdit, Classes;

type
  TfrmSettings = class(TForm)
    vleSettings: TValueListEditor;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    procedure ReadSettings;
    procedure WriteSettings;
  public
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

uses
  ForestConsts, IniFiles, NsUtils;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TfrmSettings }

procedure TfrmSettings.btnOkClick(Sender: TObject);
begin
  WriteSettings();
end;

//---------------------------------------------------------------------------

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  ReadSettings();
end;

//---------------------------------------------------------------------------

procedure TfrmSettings.ReadSettings;
begin
  vleSettings.Strings.Clear();

  vleSettings.InsertRow(S_SETTINGS_TEMPLATE_SELECT, SQLSelectTemplate, True);
  vleSettings.InsertRow(S_SETTINGS_TEMPLATE_INSERT, SQLInsertTemplate, True);
  vleSettings.InsertRow(S_SETTINGS_TEMPLATE_UPDATE, SQLUpdateTemplate, True);
  vleSettings.InsertRow(S_SETTINGS_TEMPLATE_DELETE, SQLDeleteTemplate, True);
  vleSettings.InsertRow(S_SETTINGS_DB_DATABASE, PgDatabase, True);
  vleSettings.InsertRow(S_SETTINGS_DB_SERVER, PgServer, True);
  vleSettings.InsertRow(S_SETTINGS_DB_PORT, PgPort, True);
  vleSettings.InsertRow(S_SETTINGS_DB_LOGIN, PgUID, True);
  vleSettings.InsertRow(S_SETTINGS_DB_PASSWORD, PgPassword, True);
  vleSettings.InsertRow(S_SETTINGS_QUERIES_REMEMBERED, IntToStr(MaxQueriesCount), True);

  vleSettings.InsertRow(S_SETTINGS_HIDE_TO_TRAY, BoolToStr(TrayEnabled), True);
  vleSettings.ItemProps[S_SETTINGS_HIDE_TO_TRAY].EditStyle := esPickList;
  vleSettings.ItemProps[S_SETTINGS_HIDE_TO_TRAY].PickList.Assign(C_LIST_YESNO);
  vleSettings.ItemProps[S_SETTINGS_HIDE_TO_TRAY].ReadOnly := True;
  vleSettings.Values[S_SETTINGS_HIDE_TO_TRAY] :=
    vleSettings.ItemProps[S_SETTINGS_HIDE_TO_TRAY].PickList.Strings[BoolToInt(TrayEnabled)];
end;

//---------------------------------------------------------------------------

procedure TfrmSettings.WriteSettings;
begin
  SQLSelectTemplate := vleSettings.Values[vleSettings.Keys[1]];
  SQLInsertTemplate := vleSettings.Values[vleSettings.Keys[2]];
  SQLUpdateTemplate := vleSettings.Values[vleSettings.Keys[3]];
  SQLDeleteTemplate := vleSettings.Values[vleSettings.Keys[4]];
  PgDatabase := vleSettings.Values[vleSettings.Keys[5]];
  PgServer := vleSettings.Values[vleSettings.Keys[6]];
  PgPort := vleSettings.Values[vleSettings.Keys[7]];
  PgUID := vleSettings.Values[vleSettings.Keys[8]];
  PgPassword := vleSettings.Values[vleSettings.Keys[9]];
  MaxQueriesCount := StrToInt(vleSettings.Values[vleSettings.Keys[10]]);
  TrayEnabled := StrToBool(vleSettings.Values[vleSettings.Keys[11]]);
end;

end.

