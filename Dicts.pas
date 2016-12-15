unit Dicts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmDicts = class(TForm)
    cmbDicts: TComboBox;
    memValues: TMemo;
    lblDictionary: TLabel;
    lblValidValues: TLabel;
    btnClear: TButton;
    btnFillFromDB: TButton;
    btnOk: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmbDictsSelect(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure Apply;
    procedure btnOkClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnFillFromDBClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetFileForDict(const Dict: AnsiString): AnsiString;
  end;

var
  frmDicts: TfrmDicts;

implementation

uses
  ForestConsts, Data, Validator;

{$R *.dfm}

//---------------------------------------------------------------------------
{ TfrmDicts }

procedure TfrmDicts.Apply;
var
  WordsList: TStringList;

begin
  WordsList := TStringList.Create;
  WordsList.Assign(memValues.Lines);

  dmData.SetValidList(GetFileForDict(cmbDicts.Text), WordsList);

  WordsList.Free();
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnApplyClick(Sender: TObject);
begin
  Apply();
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnClearClick(Sender: TObject);
begin
  memValues.Clear();
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnFillFromDBClick(Sender: TObject);
begin
  memValues.Lines.Assign(dmData.GetValuesFromTable(GetFileForDict(cmbDicts.Text)));
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnOkClick(Sender: TObject);
begin
  Apply();
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.cmbDictsSelect(Sender: TObject);
begin
  memValues.Lines.Clear();
  memValues.Lines.Assign(dmData.GetValidList(GetFileForDict(cmbDicts.Text)));
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.FormCreate(Sender: TObject);
begin
  cmbDicts.Clear();
  cmbDicts.Items.Add(S_DICTIONARY_FORESTRIES_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_LOCAL_FORESTRIES_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_LANDUSE_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_PROTECT_CATEGORY_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_SPECIES_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_DAMAGE_NAME);
  cmbDicts.Items.Add(S_DICTIONARY_PEST_NAME);
  cmbDicts.ItemIndex := 0;
  cmbDictsSelect(Sender);
end;

//---------------------------------------------------------------------------

function TfrmDicts.GetFileForDict(const Dict: AnsiString): AnsiString;
begin
  Result := S_DICTIONARY_VALID_PREFIX;

  if Dict = (S_DICTIONARY_FORESTRIES_NAME) then
    Result := Result + S_DICTIONARY_FORESTRIES_FILE

  else if Dict = (S_DICTIONARY_LOCAL_FORESTRIES_NAME) then
    Result := Result + S_DICTIONARY_LOCAL_FORESTRIES_FILE

  else if Dict = (S_DICTIONARY_LANDUSE_NAME) then
    Result := Result + S_DICTIONARY_LANDUSE_FILE
   
  else if Dict = (S_DICTIONARY_PROTECT_CATEGORY_NAME) then
    Result := Result + S_DICTIONARY_PROTECT_CATEGORY_FILE

  else if Dict = (S_DICTIONARY_SPECIES_NAME) then
    Result := Result + S_DICTIONARY_SPECIES_FILE

  else if Dict = (S_DICTIONARY_DAMAGE_NAME) then
    Result := Result + S_DICTIONARY_DAMAGE_FILE

  else if Dict = (S_DICTIONARY_PEST_NAME) then
    Result := Result + S_DICTIONARY_PEST_FILE;
end;

end.

