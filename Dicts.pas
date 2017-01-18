unit Dicts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ForestTypes;

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
    edtShowFormat: TEdit;
    lblShowFormat: TLabel;
    btnFormatHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmbDictsSelect(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnFillFromDBClick(Sender: TObject);
    procedure btnFormatHelpClick(Sender: TObject);
  private
    { Private declarations }
    FWordsArr: TDictArr;
    procedure Apply;
  public
    { Public declarations }
    function GetFileForDict(const DictCaption: AnsiString): AnsiString;
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
begin
  dmData.Validator.GetDictionary(cmbDicts.Text).DictionaryArr := FWordsArr;
  dmData.Validator.GetDictionary(cmbDicts.Text).DictionaryFormatString :=
    edtShowFormat.Text;
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
var
  I: Integer;

begin
  memValues.Lines.Clear();

  FWordsArr := dmData.GetValuesFromTable(
    dmData.Validator.GetDictionary(cmbDicts.Text).Caption);

  for I := 0 to Length(FWordsArr) - 1 do
    memValues.Lines.Add(FWordsArr[I].WordValue);
    
  memValues.SelStart := 0;
  memValues.SelLength := 0;
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnFormatHelpClick(Sender: TObject);
begin
  ShowMessage(S_FORMAT_HELP);
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.btnOkClick(Sender: TObject);
begin
  Apply();
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.cmbDictsSelect(Sender: TObject);
var
  I: Integer;

begin
  memValues.Lines.Clear();

  FWordsArr := dmData.Validator.GetDictionary(cmbDicts.Text).DictionaryArr;

  for I := 0 to Length(FWordsArr) - 1 do
    memValues.Lines.Add(FWordsArr[I].WordValue);

  memValues.SelStart := 0;
  memValues.SelLength := 0;

  edtShowFormat.Text := dmData.Validator.GetDictionary(cmbDicts.Text).DictionaryFormatString;
end;

//---------------------------------------------------------------------------

procedure TfrmDicts.FormCreate(Sender: TObject);
begin
  cmbDicts.Clear();
  // Yes, this! Because i can modify caption later
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_FORESTRIES_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_LOCAL_FORESTRIES_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_LANDUSE_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_PROTECT_CATEGORY_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_SPECIES_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_DAMAGE_NAME).Caption);
  cmbDicts.Items.Add(dmData.Validator.GetDictionary(S_DICT_PEST_NAME).Caption);
  cmbDicts.ItemIndex := 0;
  cmbDictsSelect(Sender);
end;

//---------------------------------------------------------------------------

function TfrmDicts.GetFileForDict(const DictCaption: AnsiString): AnsiString;
begin
  Result := dmData.Validator.GetDictionary(DictCaption).DictionaryFile;
end;

end.

